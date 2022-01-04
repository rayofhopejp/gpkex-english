import os

class MakeEachFile:
    def __init__(self,filename,dirname):
        self.filename=filename
        self.readfile = open(self.filename+".txt","r")
        self.alldata=self.readfile.readlines()
        self.readfile.close()
        self.dirname=dirname+"_text/"
        os.mkdir(self.dirname)
    def run(self):
        for i,data in enumerate(self.alldata):
            out_text_file=open(self.dirname+"{:06}.txt".format(i),"w")
            text=data.strip()
            out_text_file.write(text+'\n')
            

mef=MakeEachFile("mr15","mr15_train")
mef.run()
mef=MakeEachFile("emnlp_news_mini","emnlp_news_mini_train")
mef.run()
mef=MakeEachFile("image_coco","image_coco_train")
mef.run()
