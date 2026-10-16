const els={};
function mk(id){return els[id]||(els[id]={id,innerHTML:'',value:'',checked:false,dataset:{},classList:{toggle(){}},style:{},
 appendChild(){},addEventListener(e,f){this._f=f},set onchange(f){this._f=f},set onclick(f){},querySelectorAll(){return []},textContent:''});}
global.document={getElementById:id=>mk(id),createElement:()=>mk('_'+Math.random())};
global.localStorage={getItem:()=>null,setItem(){}};
global.Chart=function(el,cfg){cfg.data.datasets.forEach(d=>{if(d.data.some(v=>v===undefined))console.log('undefined in',el.id,d.label)});this.destroy=()=>{}};
const fs=require('fs');eval(fs.readFileSync('/tmp/dash_data.js','utf8')+'\n'+fs.readFileSync('/tmp/dash_last.js','utf8'));
for(const k in els){ if(els[k]._f) els[k]._f(); }
console.log('ok; gt rows', (els.gt.innerHTML.match(/<tr/g)||[]).length, 'ptab rows',(els.ptab.innerHTML.match(/<tr/g)||[]).length, 'amt',(els.amt.innerHTML.match(/<tr/g)||[]).length);
console.log(els.overview.innerHTML.slice(0,600));
