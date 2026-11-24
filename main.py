import json
from os import environ
from urllib.request import urlopen
U=environ.get('MAKEFUN_ENV_URL')
def post(p,b):
 return json.load(urlopen(U+p,data=json.dumps(b).encode()))
V=(0,100,320,330,500,900,0)
def _d(t):return [(ord(c)-60)*5 for c in t]
Z=(0,)*32
PST=(0,_d('<<<<=>>9=;:<<<<@==>A>>@BFFFF<<<<'),_d('246648<<6<>?6=?@6<?@6=>?48<=2466'),Z,Z,Z,_d('64426442644264428664:888@@<<@B><'))
B=[0]*128;S=1;C=0;E=-1;KG=[116,4]
BD=(-17,-15,15,17);RD=(-16,-1,1,16);KD=BD+RD;ND=(18,14,33,31,-18,-14,-33,-31)
def setfen(fen):
 global S,C,E
 a=fen.split();B[:]=[0]*128;r=7;f=0
 for ch in a[0]:
  if ch=='/':r-=1;f=0
  elif ch.isdigit():f+=int(ch)
  else:
   t='pnbrqk'.index(ch.lower())+1;B[r*16+f]=t if ch.isupper() else -t
   if t==6:KG[ch.isupper()]=r*16+f
   f+=1
 S=1-2*(a[1]=='b')
 C=sum({'K':1,'Q':2,'k':4,'q':8}.get(ch,0) for ch in a[2])
 E=-1 if a[3]=='-' else (int(a[3][1])-1)*16+ord(a[3][0])-97
def atk(s,by):
 for ds,ty,sl in ((ND,2,0),(KD,6,0),(BD,3,1),(RD,4,1),((-15*by,-17*by),1,0)):
  for d in ds:
   t=s+d
   while not t&0x88 and not B[t]:
    if not sl:break
    t+=d
   if not t&0x88 and (B[t]==ty*by or (sl and B[t]==5*by)):return 1
 return 0
def moves(cap=0):
 ms=[];ap=ms.append;lr=7 if S>0 else 0
 def pm(s,t):
  if (t>>4)==lr:
   for pr in (5,4,3,2):ap(s|t<<7|pr<<14)
  else:ap(s|t<<7)
 for s in range(128):
  p=B[s]
  if p*S<=0:continue
  ty=abs(p)
  if ty==1:
   fw=16*S
   for d in (fw-1,fw+1):
    t=s+d
    if not t&0x88 and (B[t]*S<0 or t==E):pm(s,t)
   t=s+fw
   if not B[t]:
    if (t>>4)==lr:pm(s,t)
    elif not cap:
     ap(s|t<<7)
     if (s>>4)==(1 if S>0 else 6) and not B[s+2*fw]:ap(s|(s+2*fw)<<7)
  elif ty==2 or ty==6:
   for d in (ND,KD)[ty==6]:
    t=s+d
    if t&0x88:continue
    if not B[t]:
     if not cap:ap(s|t<<7)
    elif B[t]*S<0:ap(s|t<<7)
   if ty==6 and not cap and s==(4 if S>0 else 116):
    for bt,sg in ((1,1),(2,-1)) if S>0 else ((4,1),(8,-1)):
     tg=s+2*sg
     if C&bt and not B[s+sg] and not B[tg] and (sg>0 or not B[s+3*sg]) and not any(atk(x,-S) for x in (s,s+sg,tg)):ap(s|tg<<7)
  else:
   for d in (BD,RD,KD)[ty-3]:
    t=s+d
    while not t&0x88 and not B[t]:
     if not cap:ap(s|t<<7)
     t+=d
    if not t&0x88 and B[t]*S<0:ap(s|t<<7)
 return ms
UN=[];CRM={0:2,7:1,112:8,119:4}
def rk(f,t,u):
 a,b=(f+3,f+1) if t>f else (f-4,f-1)
 if u:a,b=b,a
 B[b]=B[a];B[a]=0
def make(m):
 global S,C,E
 f=m&127;t=(m>>7)&127;pr=m>>14;p=B[f];cp=B[t];ep=0;eps=-1
 if abs(p)==1 and t==E and (f&7)!=(t&7):
  eps=t-16*S;ep=B[eps];B[eps]=0
 UN.append((m,cp,ep,eps,C,E))
 B[t]=pr*S if pr else p;B[f]=0
 if abs(p)==6:
  KG[S>0]=t
  if abs(t-f)==2:rk(f,t,0)
  C&=12 if S>0 else 3
 C&=~(CRM.get(f,0)|CRM.get(t,0))
 E=f+16*S if abs(p)==1 and t-f==32*S else -1
 S=-S
 if atk(KG[S<0],S):
  unmake();return 0
 return 1
def unmake():
 global S,C,E
 m,cp,ep,eps,C,E=UN.pop();S=-S
 f=m&127;t=(m>>7)&127;pr=m>>14
 B[f]=S if pr else B[t];B[t]=cp
 if eps>=0:B[eps]=ep
 if abs(B[f])==6:
  KG[S>0]=f
  if abs(t-f)==2:rk(f,t,1)
def ev():
 s=0
 for sq in range(128):
  p=B[sq]
  if p:
   r=sq>>4;f=sq&7;mf=min(f,7-f);q=abs(p)
   s+=(V[q]+PST[q][(7-r,r)[p>0]*4+mf])*(p//q)
 return s*S
class TO(Exception):pass
def search(d,a,b,ply):
 global NN,BM
 NN+=1
 if NN>60000:raise TO
 if d<=0:
  if atk(KG[S>0],-S):d=1
  else:
   a=max(a,ev())
   if a>=b:return b
   d=-1
 n=0
 for m in sorted(moves(d<0),key=lambda m:10*V[abs(B[(m>>7)&127])]-V[abs(B[m&127])]+8*V[m>>14],reverse=True):
  if not make(m):continue
  n+=1;sc=-search(d-1,-b,-a,ply+1);unmake()
  if sc>a:
   a=sc
   if ply==0:BM=m
   if a>=b:return b
 if not n and d>=0:return -100000+ply if atk(KG[S>0],-S) else 0
 return a
def choose():
 global NN,BM
 NN=0;bm=0
 try:
  for d in range(1,99):
   search(d,-100000,100000,0);bm=BM
 except TO:pass
 return bm
if U:
 while 1:
  g=post('/game',{})
  if g['status']=='done':break
  while g['status']=='your_move':
   setfen(g['fen']);lm=g['legal_moves']
   m=choose() or 0;t=(m>>7)&127
   u=chr(97+(m&7))+chr(49+(m>>4&7))+chr(97+(t&7))+chr(49+(t>>4))+('nbrq'[(m>>14)-2] if m>>14 else '')
   if u not in lm:u=lm[0]
   g=post('/move',{'game':g['game'],'move':u})
