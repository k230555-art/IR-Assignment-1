#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <dirent.h>
#include "include/json.hpp"

using json = nlohmann::json;

//Porter Stemmer
static bool isVowel(const std::string& w, int i){
    char c=w[i];
    if(c=='a'||c=='e'||c=='i'||c=='o'||c=='u') return true;
    if(c=='y'&&i>0) return !isVowel(w,i-1);
    return false;
}
static int measure(const std::string& s){
    int m=0; bool inV=false;
    for(int i=0;i<(int)s.size();++i){
        if(isVowel(s,i)){inV=true;}
        else if(inV){++m;inV=false;}
    }
    return m;
}
static bool hasVowel(const std::string& s){
    for(int i=0;i<(int)s.size();++i) if(isVowel(s,i)) return true;
    return false;
}
static bool endsCVC(const std::string& w){
    int n=w.size();
    if(n<3) return false;
    char c3=w[n-1];
    return !isVowel(w,n-3)&&isVowel(w,n-2)&&!isVowel(w,n-1)
           &&c3!='w'&&c3!='x'&&c3!='y';
}
static bool hasSuffix(const std::string& w,const std::string& sfx,std::string& stem){
    if(w.size()<sfx.size()) return false;
    if(w.compare(w.size()-sfx.size(),sfx.size(),sfx)==0){
        stem=w.substr(0,w.size()-sfx.size()); return true;
    }
    return false;
}
std::string porterStem(const std::string& word){
    std::string w=word;
    if(w.size()<=2) return w;
    std::string stem;
    if(hasSuffix(w,"sses",stem)) w=stem+"ss";
    else if(hasSuffix(w,"ies",stem)) w=stem+"i";
    else if(!hasSuffix(w,"ss",stem)&&hasSuffix(w,"s",stem)) w=stem;
    bool extra=false;
    if(hasSuffix(w,"eed",stem)){if(measure(stem)>0) w=stem+"ee";}
    else if(hasSuffix(w,"ed",stem)){if(hasVowel(stem)){w=stem;extra=true;}}
    else if(hasSuffix(w,"ing",stem)){if(hasVowel(stem)){w=stem;extra=true;}}
    if(extra){
        std::string s2;
        if(hasSuffix(w,"at",s2)) w+="e";
        else if(hasSuffix(w,"bl",s2)) w+="e";
        else if(hasSuffix(w,"iz",s2)) w+="e";
        else{
            int n=w.size();
            if(n>1&&w[n-1]==w[n-2]&&!(w[n-1]=='l'||w[n-1]=='s'||w[n-1]=='z')&&!isVowel(w,n-1))
                w=w.substr(0,n-1);
            else if(measure(w)==1&&endsCVC(w)) w+="e";
        }
    }
    if(hasSuffix(w,"y",stem)&&hasVowel(stem)) w=stem+"i";
    struct{const char*s;const char*r;} s2[]={
        {"ational","ate"},{"tional","tion"},{"enci","ence"},{"anci","ance"},
        {"izer","ize"},{"abli","able"},{"alli","al"},{"entli","ent"},
        {"eli","e"},{"ousli","ous"},{"ization","ize"},{"ation","ate"},
        {"ator","ate"},{"alism","al"},{"iveness","ive"},{"fulness","ful"},
        {"ousness","ous"},{"aliti","al"},{"iviti","ive"},{"biliti","ble"},
    };
    for(auto& p:s2) if(hasSuffix(w,p.s,stem)&&measure(stem)>0){w=stem+p.r;break;}
    struct{const char*s;const char*r;} s3[]={
        {"icate","ic"},{"ative",""},{"alize","al"},{"iciti","ic"},
        {"ical","ic"},{"ful",""},{"ness",""},
    };
    for(auto& p:s3) if(hasSuffix(w,p.s,stem)&&measure(stem)>0){w=stem+p.r;break;}
    const char* s4[]={"al","ance","ence","er","ic","able","ible","ant",
                      "ement","ment","ent","ion","ou","ism","ate","iti",
                      "ous","ive","ize"};
    for(auto sfx:s4)
        if(hasSuffix(w,sfx,stem)){
            if(std::string(sfx)=="ion"){
                if(!stem.empty()&&(stem.back()=='s'||stem.back()=='t')&&measure(stem)>1){w=stem;break;}
            } else if(measure(stem)>1){w=stem;break;}
        }
    if(hasSuffix(w,"e",stem)){
        int m=measure(stem);
        if(m>1) w=stem;
        else if(m==1&&!endsCVC(stem)) w=stem;
    }
    {int n=w.size();
     if(n>1&&w[n-1]=='l'&&w[n-2]=='l'&&measure(w)>1) w=w.substr(0,n-1);}
    return w;
}
//End Porter Stemmer

static std::vector<std::string> extractWords(const std::string& line){
    std::vector<std::string> words;
    std::string cur;
    for(char c:line){
        if(std::isalpha((unsigned char)c)) cur+=c;
        else if(!cur.empty()){words.push_back(cur);cur.clear();}
    }
    if(!cur.empty()) words.push_back(cur);
    return words;
}

static int getNumber(const std::string& filename){
    auto pos=filename.find('_');
    if(pos==std::string::npos) return 0;
    auto dot=filename.find('.',pos);
    std::string num=filename.substr(pos+1, dot==std::string::npos ? std::string::npos : dot-pos-1);
    try{ return std::stoi(num); } catch(...){ return 0; }
}

static std::vector<std::string> listFiles(const std::string& dirPath){
    std::vector<std::string> files;
    DIR* dir=opendir(dirPath.c_str());
    if(!dir){ std::cerr<<"Cannot open directory: "<<dirPath<<"\n"; return files; }
    struct dirent* entry;
    while((entry=readdir(dir))!=nullptr){
        std::string name=entry->d_name;
        if(name=="."||name=="..") continue;
        std::string fullPath=dirPath+"/"+name;
        std::ifstream test(fullPath);
        if(test.good()) files.push_back(name);
    }
    closedir(dir);
    return files;
}

int main(){
    std::set<std::string> stopWords;
    {
        std::ifstream f("Stopword-List.txt");
        if(!f.is_open()){ std::cerr<<"Cannot open Stopword-List.txt\n"; return 1; }
        std::string line;
        while(std::getline(f,line)){
            line.erase(0,line.find_first_not_of(" \t\r\n"));
            if(line.empty()) continue;
            line.erase(line.find_last_not_of(" \t\r\n")+1);
            if(line.empty()) continue;
            stopWords.insert(porterStem(line));
        }
    }

    std::map<std::string,std::vector<int>>                       index;
    std::map<std::string,std::map<std::string,std::vector<int>>> posIdx;
    std::map<std::string,std::string>                            docMap;

    // Collect and sort speech files
    std::string dirPath="Trump Speechs";
    std::vector<std::string> filenames=listFiles(dirPath);
    std::sort(filenames.begin(),filenames.end(),[](const std::string& a,const std::string& b){
        return getNumber(a)<getNumber(b);
    });

    for(int docId=0;docId<(int)filenames.size();++docId){
        std::string docIdStr=std::to_string(docId);
        docMap[docIdStr]=filenames[docId];

        std::string fullPath=dirPath+"/"+filenames[docId];
        std::ifstream f(fullPath);
        if(!f.is_open()){ std::cerr<<"Cannot open "<<fullPath<<"\n"; continue; }

        int globalPos=0;
        std::string line;
        while(std::getline(f,line)){
            line.erase(0,line.find_first_not_of(" \t\r\n"));
            if(line.empty()) continue;
            line.erase(line.find_last_not_of(" \t\r\n")+1);
            if(line.empty()) continue;

            auto words=extractWords(line);
            for(int localPos=0;localPos<(int)words.size();++localPos){
                std::string w=words[localPos];
                std::transform(w.begin(),w.end(),w.begin(),::tolower);
                std::string stemmed=porterStem(w);
                if(stopWords.count(stemmed)) continue;

                int pos=globalPos+localPos;
                auto& docList=index[stemmed];
                if(docList.empty()||docList.back()!=docId)
                    docList.push_back(docId);
                posIdx[stemmed][docIdStr].push_back(pos);
            }
            globalPos+=(int)words.size();
        }
    }

    // Sort posting lists
    for(auto& kv:index) std::sort(kv.second.begin(),kv.second.end());
    for(auto& kv:posIdx)
        for(auto& dv:kv.second)
            std::sort(dv.second.begin(),dv.second.end());

    // Write JSON files
    {
        json j=json::object();
        for(auto& kv:index) j[kv.first]=kv.second;
        std::ofstream out("inverted_index.json");
        out<<j.dump(4)<<"\n";
    }
    {
        json j=json::object();
        for(auto& kv:posIdx){
            j[kv.first]=json::object();
            for(auto& dv:kv.second) j[kv.first][dv.first]=dv.second;
        }
        std::ofstream out("positional_index.json");
        out<<j.dump(4)<<"\n";
    }
    {
        json j=json::object();
        for(auto& kv:docMap) j[kv.first]=kv.second;
        std::ofstream out("doc_map.json");
        out<<j.dump(4)<<"\n";
    }

    std::cout<<"Indexing complete.\n";
    return 0;
}