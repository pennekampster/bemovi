---
layout: page
title: Technical considerations and compatibility
description: Hardware requirements
---

Bemovi was tested on Windows 7 & 8, as well as Mac OS X and Ubuntu 14.04.

Two technical aspects require careful consideration before embarking
on a significant project using bemovi (or any other project involving video analyses
methods). First, videos take up a considerable amount of storage space,
especially when uncompressed formats are required for analysis in
ImageJ. Using lossless uncompressed formats such as PNG avi files
is recommended, and when analysis are final, compression into even
higher compressed formats may be advised for long-term storage.

As well as sufficient storage, fast data transfer among
storage devices is desirable. Second, video analysis also requires considerable
amounts of RAM and fast CPUs, especially for the tracking (all analyses
in this example were performed on machines with at least 32GB RAM and a 3.4 Ghz
Intel Core i7 processor, though less would probably have sufficed).