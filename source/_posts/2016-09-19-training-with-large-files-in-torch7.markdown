---
layout: post
title: "Training with large files in Torch"
date: 2016-09-19 17:50:12 +0530
comments: true
categories: 
- torch
- lua
- io
- big-data
---

Datasets these days are getting larger and and so is hardware in keeping with [Moore's law][moore-law-wiki]. However, Moore's law applies to population trends. For a given user, budget constraints create a situation where hardware resources increase as a step function. On the other hand, the growth function for datasizes can be approximated with a much smoother exponential. This keeps users typically chasing their own hyper-personal version of [big data][big-data-wiki].

I recently started using [lua][lua-wiki] and [torch][torch] to learn about neural networks. I wanted to start simple and build a classifier for a kaggle competition. However, the data for this competition is too _big_ for my machine with a measly 16 gigs of RAM. [This used to be a luxury only half a decade ago.]

So, after some digging around on github, I figured how to train with fairly large datasets with stock torch infrastructure and I'm gonna show you how and why it works.

<!--more-->

# How did you do that?

## Hold up, what all did you try?

### csv2tensor

The file that I have is 2 gigs on disk. So, my first attempt, obviously, was to throw caution to the wind and attempt to load the dataset anyway. To this end, I tried [csv2tensor][csv2tensor]. This was a no-go right from the get-go (you see what I did there?).

This library converts the columns in the data into lua `tables` and then converts those `tables` to torch `Tensors`. The rub is that tables in lua are not allocated in user memory but in the compiler's memory which has a small [upper-bound][lua-mem-lim] because in [luajit][luajit] that torch is typically compiled against. This means that using this library will blow up for any dataset which expands to a more than a gig or so.

### csvigo

[`csvigo`][lua-csv] is the standard library for working with csv files in lua / torch7. Again, my first attempt was to read the whole data into memory with this package. I opened `top` in a split and proceeded to load the dataset. While this library did not run into the issue above (I think because it uses `Tensors` directly which are allocated in user-space), it quickly ate up all the available memory and a few gigs of swap before I killed the process. I tried to `setdefaulttensortype` to `torch.FloatTensor` which I killed after waiting for under a minute by when it had clocked 7 gigs.

## The solution: csvigo with <em>mode=large</em>

At this point, I looked at the csvigo documentation again and found the [large-mode][large-csv-mode] option. I decided to try it and was able to successfully put together this solution:

```lua

local numericData = csvigo.load({
      path = dataDir .. "numeric.csv",
      mode = 'large'
})

-- Use a level of indirection to get at the records
local trainingData = {}

-- Discount 1 for the header row
function trainingData:size() return #numericData - 1 end

--[[
`preprocess` is an arbitrary function that operates on a row of data.
Implement it (or remove it) as per your requirements.
Assumption: The response is at the last index in the record
--]]
setmetatable(trainingData,
            {
                __index = function (tbl, ind)
                   local row = preprocess(numericData[ind + 1])
                   local record = torch.Tensor(row)
                   local response = record[-1]
                   local penultimate = record:size(1) - 1
                   local rest = record[{ {1, penultimate} }]
                   return {rest, response}
                end
});

-- Trainer
----------

-- Let `mlp` be our nn model

local params, gradParams = mlp:getParameters()
local optimState = {learningRate = 1}

-- Training
-----------

local nCols = #(numericData[1]) - 1
local nEpochs = 32
local batchSize = 2048

for epoch = 1, nEpochs do

   local shuffle = torch.randperm(trainingData:size())

   local batch = 1
   for batchOffset = 1, trainingData:size(), batchSize do

      local batchInputs = torch.Tensor(batchSize, nCols)
      local batchResponse = torch.Tensor(batchSize)
      for i = 1, batchSize do
         local thisRecord = trainingData[shuffle[batchOffset + i]]
         batchInputs[i]:copy(thisRecord[1])
         batchResponse[i] = thisRecord[2] + 1
      end

      local function evaluateBatch(params)
         gradParams:zero()
         local batchEstimate = mlp:forward(batchInputs)
         local batchLoss = criterion:forward(batchEstimate, batchResponse)
         local nablaLossOutput = criterion:backward(
            batchEstimate, batchResponse)
         mlp:backward(batchInputs, nablaLossOutput)
         print('Finished epoch: ' .. epoch .. ', batch: ' ..
                  batch .. ', with loss: ' .. batchLoss)
         return batchLoss, gradParams
      end

      optim.sgd(evaluateBatch, params, optimState)

      batch = batch + 1
      batchOffset = batchOffset + batchSize

   end

end
```

With this method I was able to use the dataset with only two gigs of memory allocated to the process. Perhaps, in another post I will benchmark the performance penalty for doing this when you do have sufficient RAM. I have also not figured out how this will need to be adapted to GPUs. But this is definitely better compared to the option of not using a moderately large dataset at all.

## What's the magic sauce? o.O

Note how the [documentation][large-csv-mode] modestly states that the efficiency is under the hood? Which is why we need to [_use the source, Luke_][utsl]. So, let's look at this (incomplete) [code][csv-source] snippet and comprehend:

```
function Csv:largereadall()
    local ok = pcall(require, 'torch')
    if not ok then
        error('large mode needs the torch package')
    end
    local libcsvigo = require 'libcsvigo'
    local ffi = require 'ffi'
    local path = self.filepath
    local f = torch.DiskFile(path, 'r'):binary()
    f:seekEnd()
    local length = f:position() - 1
    f:seek(1)
    local data = f:readChar(length)
    f:close()

    -- now that the ByteStorage is constructed,
    -- one has to make a dictionary of [offset, length] pairs of the row.
    -- for efficiency, do one pass to count number of rows,
    -- and another pass to create a LongTensor and fill it
    local lookup = libcsvigo.create_lookup(data)

    local out = {}
    local separator = self.separator

    local function index (tbl, i)
        assert(i, 'index has to be given')
        assert(i > 0 and i <= lookup:size(1), "index out of bounds: " ..  i)
        local line = ffi.string(data:data() + lookup[i][1], lookup[i][2])
        local entry = fromcsv(line, separator)
        return entry
    end
```

1. **Loading to ByteStorage**: The first interesting thing that the code does is to load the file as raw bytes. This is the part of code spanning `torch.DiskFile(path, 'r'):binary()` to `f:close()`. The intermediate steps are only to calculate the length of the content in bytes. This makes the data take only as much space in memory as it does on the disk (remember how my 2GB file took only 2 gigs in RAM?).

2. **Calculating the byte offset and the byte length of each new record in the file**: The `libcsvigo.create_lookup(data)` essentially scans the byte representation of the file and records at what offset from the beginning is the first byte of each record. Additionally, it also stores how many bytes are in each record -- the byte length. These are stored as pairs in the `lookup` table.

3. **Accessing the `i`th record in the file**: When it's time to access the `i`th row in the file, the function `index` creates a string by reading in `lookup[i][2]` number of bytes starting from `lookup[i][1]`. The `fromcsv` function then splits this record string into a `table` by splitting it on the separator and returns it us.

This allows us to load large datasets in memory employing only as much memory as is the size of the file on disk (in bytes) with the additional memory cost of creating the `LongTensor` lookup table. There is, obviously, the additional CPU cost of processing each record as many times as it is read.

<!--links-->
[moore-law-wiki]: https://en.wikipedia.org/wiki/Moore%27s_law
[big-data-wiki]: https://en.wikipedia.org/wiki/Big_data
[lua-wiki]: https://en.wikipedia.org/wiki/Lua_(programming_language)
[torch]: http://torch.ch/
[csv2tensor]: https://github.com/willkurt/csv2tensor
[luajit]: http://luajit.org/
[lua-mem-lim]: http://kvitajakub.github.io/2016/03/08/luajit-memory-limitations/ 
[lua-csv]: https://github.com/clementfarabet/lua---csv/
[large-csv-mode]: https://github.com/clementfarabet/lua---csv/#large-csv-mode
[utsl]: http://www.catb.org/jargon/html/U/UTSL.html
[csv-source]: https://github.com/clementfarabet/lua---csv/blob/master/File.lua
