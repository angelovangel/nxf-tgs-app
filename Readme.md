### nxf-tgs-app
A Shiny app to run the nxf-tgs pipeline (instead of EPI2ME)

### Notes
- run as `tmux` process - use ont-plasmid-app logic
- fastq_pass is local to server, samplesheet is uploaded
-  

```
nohup miniserve -I -i 10.74.138.144 /path/to/nxf-tgs-app-storage &
```