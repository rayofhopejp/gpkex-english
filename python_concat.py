import os

class ConcatFile:
    def __init__(self,keydirname,filename):
        self.keydirname=keydirname
        self.filename=filename
    def run(self):
        files_paths=os.listdir(self.keydirname)
        write_file_handle=open(self.filename+".txt","w")
        files_paths.sort()
        for file_path in files_paths:
            if file_path[-4:]==".txt":
                file_handle=open(self.keydirname+file_path,"r",encoding="utf8", errors='ignore')
                keywords=file_handle.read().strip().split("\n")
                write_file_handle.write(" ".join(keywords)+"\n")
        
            

cof=ConcatFile("m_generatedkeyword/","mr15_keywords")
cof.run()
cof=ConcatFile("e_generatedkeyword/","emnlp_news_mini_keywords")
cof.run()
cof=ConcatFile("ic_generatedkeyword/","image_coco_keywords")
cof.run()

