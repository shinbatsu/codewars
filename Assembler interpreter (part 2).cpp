#include <string>
#include <cstring>
#include <cctype>
#include <cstdlib>
using namespace std;

string assembler_interpreter(string p) {
    static constexpr int sd=16;
    const char *ip=p.c_str(), *cs[sd]={};
    string o;
    int r[26]={}, lt=0, eq=0, gt=0, sp=sd-1, x=0,y=0,v=0;

    auto ws=[](int c){return isblank(c);};
    auto num=[](int c){return isdigit(c);};
    auto low=[](int c){return islower(c);};
    auto noteol=[](int c){return c&&c!='\n';};
    auto notcolon=[](int c){return c&&c!=':'&&!isspace(c);};
    auto islabel=[](int c){return isalnum(c)||c=='_';};

    auto skip=[&](int n,auto&&eq){
        while(n-->0&&*ip)++ip;
        while(eq(*ip))++ip;
    };
    auto reg=[&](){return *ip++-'a';};
    auto val=[&](){
        if(low(*ip))return r[reg()];
        v=atoi(ip);
        skip(1,num);
        return v;
    };
    auto outs=[&](const char* b,const char* e){o.append(b,e);};
    auto outv=[&](int v){o+=to_string(v);};
    auto out=[&](){
        if(*ip=='\''){
            ++ip;
            const char* e=strchr(ip,'\'');
            if(!e)e=ip+strlen(ip);
            outs(ip,e);
            ip=e+1;
        }else outv(val());
    };
    auto jump=[&](const char* ls){
        const char* l=ip;
        skip(0,islabel);
        string lbl("\n"); lbl.append(l,ip-l); lbl+=':';
        const char* f=strstr(ls,lbl.c_str());
        if(f)ip+=f+2-l;
    };

    while(*ip){
        skip(0,ws);
        if(!strncmp(ip,"mov ",4)){
            skip(4,ws);
            int rr=reg();
            skip(1,ws);
            r[rr]=val();
        }else if(!strncmp(ip,"inc ",4)){
            skip(4,ws);
            ++r[reg()];
        }else if(!strncmp(ip,"dec ",4)){
            skip(4,ws);
            --r[reg()];
        }else if(!strncmp(ip,"add ",4)){
            skip(4,ws);
            int rr=reg();
            skip(1,ws);
            r[rr]+=val();
        }else if(!strncmp(ip,"sub ",4)){
            skip(4,ws);
            int rr=reg();
            skip(1,ws);
            r[rr]-=val();
        }else if(!strncmp(ip,"mul ",4)){
            skip(4,ws);
            int rr=reg();
            skip(1,ws);
            r[rr]*=val();
        }else if(!strncmp(ip,"div ",4)){
            skip(4,ws);
            int rr=reg();
            skip(1,ws);
            r[rr]/=val();
        }else if(!strncmp(ip,"jmp ",4)){
            skip(4,ws);
            jump(p.c_str());
        }else if(!strncmp(ip,"cmp ",4)){
            skip(4,ws);
            x=val();
            skip(1,ws);
            y=val();
            lt=x<y; eq=x==y; gt=x>y;
        }else if(!strncmp(ip,"jne ",4)){
            skip(4,ws);
            if(!eq) jump(p.c_str());
        }else if(!strncmp(ip,"je ",3)){
            skip(3,ws);
            if(eq) jump(p.c_str());
        }else if(!strncmp(ip,"jge ",4)){
            skip(4,ws);
            if(!lt) jump(p.c_str());
        }else if(!strncmp(ip,"jg ",3)){
            skip(3,ws);
            if(gt) jump(p.c_str());
        }else if(!strncmp(ip,"jle ",4)){
            skip(4,ws);
            if(!gt) jump(p.c_str());
        }else if(!strncmp(ip,"jl ",3)){
            skip(3,ws);
            if(lt) jump(p.c_str());
        }else if(!strncmp(ip,"call ",5)){
            skip(5,ws);
            if(!sp) break;
            cs[--sp]=1+strchr(ip,'\n');
            jump(p.c_str());
        }else if(!strncmp(ip,"msg ",4)){
            ip+=3;
            do{
                skip(1,ws);
                out();
            }while(*ip==',');
        }else if(!strncmp(ip,"ret",3)&&(isspace(ip[3])||!ip[3])){
            ip=cs[sp++];
            if(sp>=sd) break;
            continue;
        }else if(!strncmp(ip,"end",3)&&(isspace(ip[3])||!ip[3])){
            return o;
        }
        skip(0,ws);
        skip(0,*ip==';'?noteol:notcolon);
        if(*ip)++ip;
    }
    return "-1";
}