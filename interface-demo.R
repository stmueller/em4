source("minimalmodel.R")
##You can also add objects one at a time:
ResetDisplay()
AddObject(1,8,.8,.4,"file")
AddObject(2,8,.8,.4,"edit")
AddObject(3,8,.8,.4,"options")
AddObject(4,8,.8,.4,"tools")
##a sort of spreadsheet
AddObject(2,4.5,.8,.4,"val1")
AddObject(2,5,.8,.4,"val1")
AddObject(2,5.5,.8,.4,"val3")
AddObject(2,6,.8,.4,"val4")
AddObject(2,6.5,.8,.4,"val5")

AddObject(3,4.5,.8,.4,"val6")
AddObject(3,5,.8,.4,"val7")
AddObject(3,5.5,.8,.4,"val8")
AddObject(3,6,.8,.4,"val9")
AddObject(3,6.5,.8,.4,"val10")

AddObject(4,4.5,.8,.4,"val10")
AddObject(4,5,.8,.4,"val11")
AddObject(4,5.5,.8,.4,"val12")
AddObject(4,6,.8,.4,"val13")
AddObject(4,6.5,.8,.4,"val14")
PlotDisplay()

out <- GetScanPath(1,5,"edit",plot=3,eccnoise=.3,foveasize=1.0)

