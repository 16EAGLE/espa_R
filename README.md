# R client for the USGS-EROS espa API

This repository includes an R client for the USGS-EROS espa ordering system API, which can be found <a href="https://github.com/USGS-EROS/espa-api">here<a/>. The contained script /espa_client/espa_example.R offers a short explanation on how to use the client. The client takes product IDs, e.g. for specific Landsat scenses, provided within an input .txt file, performs a check of there vadility, places the required orders, indexes and waits the server-side processing status and downloads and MD5-checksums the products when they are ready.

The client does not offer querying by search parameters such as date or region, since espa is build for on-demand pre-processing. For further information, please be referred to <a href="https://github.com/USGS-EROS/espa-api/issues/93">this explanation by USGS-EROS</a>.

This client's functionality is integrated into the <a href="https://github.com/16eagle/getSpatialData">getSpatialData<a/> package for R, which allows you to not only use USGS-EROS ESPA as a source for Landsat data, but also Amazon AWS.

<a href="https://github.com/16eagle/getSpatialData">https://github.com/16eagle/getSpatialData<a/>.