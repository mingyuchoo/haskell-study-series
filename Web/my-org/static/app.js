'use strict';
let state, organizations=[], selectedOrg=null, view='organizations', guideOpen=true, deletionSnapshot=null, deleting=false, stateIsFresh=false, requestToken=0, contextToken=0;
const content=document.querySelector('#content');
const permissions=['Pricing','Hiring','BudgetApproval','Contracting','Marketing','Infrastructure','ProductLaunch'];
const names={Pricing:'가격 결정',Hiring:'채용',BudgetApproval:'예산 승인',Contracting:'계약',Marketing:'마케팅',Infrastructure:'인프라',ProductLaunch:'제품 출시'};
const statusNames={NoData:'결과 대기',OnTrack:'정상',AtRisk:'위험',OffTrack:'이탈',Achieved:'달성'};
const esc=x=>String(x??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const number=x=>Number(x??0).toLocaleString('ko-KR',{maximumFractionDigits:2});
const date=x=>new Date(x).toLocaleDateString('ko-KR');
const uid=prefix=>prefix+'-'+crypto.randomUUID().slice(0,8);
const person=id=>state.people.find(p=>p.id===id)?.name||id||'미지정';
const auth=id=>state.authorities.find(a=>a.owner===id);
const personOptions=selected=>'<option value="">구성원 선택</option>'+state.people.map(p=>`<option value="${esc(p.id)}" ${p.id===selected?'selected':''}>${esc(p.name)} · ${esc(p.role)}</option>`).join('');
const goalsOptions=()=>state.goals.map(x=>`<option value="${esc(x.goal.id)}">${esc(x.goal.description)}</option>`).join('');
const input=(label,name,type='text',value='',required=true)=>`<label>${label}<input name="${name}" type="${type}" value="${esc(value)}" ${required?'required':''} ${type==='number'?'step="any"':''}></label>`;
const select=(label,name,options)=>`<label>${label}<select name="${name}" required>${options}</select></label>`;
const checks=selected=>`<div class="checks">${permissions.map(p=>`<label><input type="checkbox" name="permissions" value="${p}" ${(selected||[]).includes(p)?'checked':''}>${names[p]}</label>`).join('')}</div>`;
const badge=x=>`<span class="tag ${!x.active?'draft':x.evaluation.status==='OffTrack'?'error':x.evaluation.status==='AtRisk'?'warn':''}">${!x.active?'초안':statusNames[x.evaluation.status]}</span>`;
function notify(message,error=false){const n=document.querySelector('#notice');n.textContent=message;n.className=error?'error':'';}
async function api(path,body,method='POST'){const r=await fetch('/api/'+path,body===undefined?{}:{method,headers:{'Content-Type':'application/json'},body:JSON.stringify(body)});const data=await r.json();if(!r.ok){const error=Error(data.error||'요청 실패');error.status=r.status;throw error;}return data;}
const orgPath=(id,tail='')=>'organizations/'+encodeURIComponent(id)+(tail?'/'+tail:'');
function currentContext(id,token){return selectedOrg===id&&contextToken===token;}
function dismissConfirmation(){deletionSnapshot=null;const dialog=document.querySelector('#delete-organization-dialog');if(dialog.open)dialog.close();}
async function navigate(next,id=null){dismissConfirmation();contextToken++;requestToken++;selectedOrg=id;view=next;state=undefined;stateIsFresh=false;notify('');render();await refresh();}
async function refresh(){
 const token=++requestToken,id=selectedOrg,context=contextToken,currentView=view;
 stateIsFresh=false;
 try{
  const data=await api(id?orgPath(id,currentView==='organization'?'':'dashboard'):'organizations');
  if(token!==requestToken||!currentContext(id,context)||view!==currentView)return false;
  if(id)state=data;else organizations=data;
  stateIsFresh=true;render();return true;
 }catch(e){
  if(token!==requestToken||!currentContext(id,context)||view!==currentView)return false;
  notify('최신 상태를 불러오지 못했습니다: '+e.message,true);return false;
 }
}
function render(){
 document.querySelector('#organization-settings').hidden=!selectedOrg||!state;
 document.querySelector('#org-name').textContent=state?.organization?.name||'조직 목록';
 document.querySelectorAll('[data-view]').forEach(b=>{b.classList.toggle('selected',b.dataset.view===view);b.disabled=b.dataset.view!=='organizations'&&(!selectedOrg||!state);});
 const titles={organizations:['조직 목록','조직을 등록하고, 각 조직의 목표와 책임을 독립적으로 관리하세요.'],organization:['조직 상세','조직 정보를 확인하고 이름을 수정하거나 조직을 삭제합니다.'],dashboard:['Goal Dashboard','모든 중요한 결과에는 명확한 책임자가 있습니다.'],responsibility:['Responsibility Map','결과, 책임자, 권한을 하나의 실행 지도로 연결합니다.'],authority:['Authority Map','책임자가 필요한 결정을 스스로 내릴 수 있는지 확인합니다.'],reviews:['Review & Learning','결과를 돌아보고, 다음 결정을 조직의 기억으로 남깁니다.']};
 document.querySelector('#title').textContent=titles[view][0];document.querySelector('#subtitle').textContent=titles[view][1];document.querySelector('#crumb').textContent=view.toUpperCase();
 if(view==='organizations')content.innerHTML=organizationList();
 else if(!state)content.innerHTML='<section class="panel"><p>조직을 불러오는 중입니다. 조회에 실패하면 새로고침하거나 조직 목록으로 돌아가세요.</p></section>';
 else if(view==='organization')content.innerHTML=organizationDetail();
 else content.innerHTML=(state.demo?demoGuide():'')+({dashboard,responsibility,authority,reviews}[view])();
 content.querySelectorAll('form').forEach(f=>{f.dataset.org=selectedOrg||'';f.dataset.context=contextToken;});
}
function organizationList(){return `<section class="panel"><span class="tag">ORGANIZATIONS</span><h2>새 조직 등록</h2><p class="muted">조직마다 구성원, 목표, 권한과 회고가 별도로 저장됩니다.</p><form data-form="organization">${input('조직 이름','name')}<button>조직 등록</button> <button type="button" class="secondary" data-action="demo" ${organizations.some(x=>x.organization.id==='demo-northstar-v2')?'disabled':''}>체험용 데모 조직 추가</button></form></section><div class="section-head"><h2>등록된 조직</h2><span class="tag">${organizations.length}개</span></div>${organizations.length?`<div class="grid">${organizations.map(x=>`<article class="panel organization-card"><span class="tag">${x.demo?'가상 데이터 · 데모':'내 조직'}</span><h2>${esc(x.organization.name)}</h2><p>구성원 ${number(x.peopleCount)}명 · 목표 ${number(x.goalCount)}개</p><small>등록 ${date(x.organization.createdAt)}</small><div class="actions"><button data-action="open-org" data-id="${esc(x.organization.id)}">이 조직 열기</button><button class="secondary" data-action="detail-org" data-id="${esc(x.organization.id)}">상세 · 수정 · 삭제</button></div></article>`).join('')}</div>`:'<section class="panel empty"><h2>첫 조직을 등록해 보세요</h2><p>위에서 조직 이름을 입력하거나 데모 조직을 추가해 운영 흐름을 체험할 수 있습니다.</p></section>'}`;}
function organizationDetail(){const o=state.organization;return `<section class="panel"><span class="tag">${state.demo?'DEMO ORGANIZATION':'ORGANIZATION'}</span><h2>${esc(o.name)}</h2><dl class="organization-meta"><dt>조직 ID</dt><dd>${esc(o.id)}</dd><dt>등록일</dt><dd>${date(o.createdAt)}</dd><dt>구성원</dt><dd>${number(state.peopleCount)}명</dd><dt>목표</dt><dd>${number(state.goalCount)}개</dd></dl><button data-action="open-org" data-id="${esc(o.id)}">이 조직 열기 →</button></section><section class="panel"><h2>조직 이름 수정</h2><form data-form="rename" data-version="${state.version}">${input('조직 이름','name','text',o.name)}<p class="note">이름만 변경하며 구성원과 목표, 기존 기록은 유지됩니다.</p><button>이름 저장</button></form></section>${organizationSettings()}`;}
function diagnosticPanel(){return `<section class="panel"><div class="section-head"><div><h2>Organization Compiler</h2><small>조직 구조에서 발견한 개선 지점</small></div><span class="tag ${state.compiler.errors?'error':'warn'}">${state.compiler.errors} 오류 · ${state.compiler.warnings} 경고</span></div>${state.compiler.diagnostics.length?state.compiler.diagnostics.map(d=>`<div class="diagnostic ${d.severity==='Error'?'error':''}"><code>${esc(d.code)}</code><strong>${esc(d.message)}</strong><p>${esc(d.subject)}</p>${d.details.map(t=>`<p>${esc(t)}</p>`).join('')}</div>`).join(''):'<p class="muted">구조 검사를 통과했습니다. 결과를 보고하고 학습을 이어가세요.</p>'}<p class="note">집중도는 권한 종류와 예산 보유를 각각 1점으로 세는 규칙 기반 추정치입니다.</p></section>`;}
function dashboard(){const active=state.goals.filter(x=>x.active).length;return `<div class="metrics">${[['전체 목표',state.goals.length,'중요한 결과'],['활성 목표',active,'책임과 권한 검증 완료'],['구조 진단',state.compiler.errors+state.compiler.warnings,'검토가 필요한 항목'],['누적 학습',state.reviews.reduce((n,r)=>n+r.learnings.length,0),'조직에 남은 발견']].map(([l,n,s])=>`<div class="metric"><span>${l}</span><strong>${n.toString().padStart(2,'0')}</strong><small>${s}</small></div>`).join('')}</div><div class="section-head"><div><h2>목표 포트폴리오</h2><small>책임에서 결과까지, 한눈에</small></div><a href="#new-goal">+ 새 목표</a></div>${state.goals.length?`<div class="grid">${state.goals.map(goalCard).join('')}</div>`:'<section class="panel empty"><h2>첫 번째 목표를 정의하세요</h2><p>업무 목록 대신 측정 가능한 결과부터 시작합니다.</p></section>'}<details class="panel" id="new-goal"><summary>+ 목표 만들기</summary>${goalForm()}</details><details class="panel"><summary>조직 구성원 추가 (${state.people.length}명)</summary><form data-form="person"><div class="fields">${input('이름','name')}${input('역할','role')}</div><button>구성원 추가</button></form></details>${diagnosticPanel()}${organizationSettings()}`;}
function goalForm(){const today=new Date().toISOString().slice(0,10),deadline=new Date(Date.now()+90*86400000).toISOString().slice(0,10);return `<form data-form="goal">${input('어떤 결과를 만들고 싶나요?','description')}<div class="fields">${input('KPI 이름','metricName')}${input('단위','unit')}${input('지표 식별자 (같은 지표는 같은 ID 사용)','metricId','text','metric-'+crypto.randomUUID().slice(0,8))}${select('좋은 결과의 방향','direction','<option value="HigherIsBetter">높을수록 좋음</option><option value="LowerIsBetter">낮을수록 좋음</option>')}${input('기준값','baseline','number',0)}${input('목표값','target','number',100)}${input('시작일','startsAt','date',today)}${input('마감일','deadline','date',deadline)}${input('필요 예산 (KRW)','budget','number',0)}<label>상위 목표<select name="parent"><option value="">없음</option>${goalsOptions()}</select></label></div><label>필요한 결정 권한</label>${checks()}<button>초안 생성</button></form>`;}
function goalCard(x){const g=x.goal,e=x.evaluation;return `<article class="goal-card" id="goal-${esc(g.id)}">${badge(x)}<h2>${esc(g.description)}</h2><small>${esc(g.metric.name)} · ${g.metric.direction==='HigherIsBetter'?'↑ 증가':'↓ 감소'} 목표</small><div class="goal-values"><strong>${e.latestValue==null?'—':number(e.latestValue)}</strong><span class="muted">/ ${number(g.target)} ${esc(g.metric.unit)}</span></div><div class="progress"><progress max="1" value="${e.progress}"></progress></div><small>${Math.round(e.progress*100)}% 달성 · 기준 ${number(g.baseline)}</small><div class="meta"><span>◉ ${esc(person(x.owner))}</span><span>${date(g.deadline)} 마감</span></div><details><summary>책임 · 권한 · 결과 관리</summary><p class="note">${esc(x.analysis.possibleCause)}</p><form data-form="owner" data-id="${esc(g.id)}">${select('단일 최종 책임자','owner',personOptions(x.owner))}<button class="secondary">책임자 지정</button></form><p class="note">권한은 Authority Map에서 편집합니다. 책임자 변경 또는 권한 부족 시 초안으로 돌아갑니다.</p><div class="actions"><button data-action="activate" data-id="${esc(g.id)}" ${x.active?'disabled':''}>${x.active?'활성화됨':'목표 활성화'}</button><button class="secondary" data-action="evaluate" data-id="${esc(g.id)}">평가 기록</button></div><form data-form="result" id="result-${esc(g.id)}" data-id="${esc(g.id)}"><div class="fields">${input('실측값','value','number')}${select('보고자','reportedBy',personOptions(x.owner))}</div>${input('결과 설명','note')}<button>결과 보고</button></form>${resultHistory(x)}<form data-form="strategy" data-id="${esc(g.id)}">${input('새로운 전략과 변경 이유','note')}<button class="secondary">전략 변경 기록</button></form>${x.strategies.map(s=>`<p class="note">${date(s[0])} · ${esc(s[1])}</p>`).join('')}</details></article>`;}
function responsibility(){return `<section class="panel"><h2>누가 어떤 결과를 책임지는가</h2><p class="muted">각 목표에는 최종 책임자가 한 명 있습니다.</p><div class="table-wrap"><table><thead><tr><th>결과 / KPI</th><th>최종 책임자</th><th>목표값</th><th>필요 권한 / 통제율</th><th>현재 상태</th></tr></thead><tbody>${state.goals.map(x=>`<tr id="owner-${esc(x.goal.id)}"><td><strong>${esc(x.goal.description)}</strong>${esc(x.goal.metric.name)}</td><td><form data-form="owner" data-id="${esc(x.goal.id)}"><select name="owner" aria-label="최종 책임자" required>${personOptions(x.owner)}</select><button class="secondary">지정</button></form></td><td>${number(x.goal.target)} ${esc(x.goal.metric.unit)}</td><td>${x.goal.requiredPermissions.map(p=>names[p]).join(' · ')||'별도 권한 없음'}<br><small>예산 ${number(x.goal.requiredBudget)}원 · ${Math.round(x.analysis.coverage*100)}% 통제</small></td><td>${badge(x)}</td></tr>`).join('')||'<tr><td colspan="5">등록된 목표가 없습니다.</td></tr>'}</tbody></table></div></section><section class="panel" id="responsibility-graph"><h2>Responsibility Graph</h2><p class="muted">사람 → 목표 → 지표, 목표의 의존 관계와 자원 통제를 연결합니다.</p>${state.graph.edges.map(e=>`<div class="graph-edge"><span>${esc(nodeName(e.from))}</span><b>─ ${esc(e.kind)} →</b><span>${esc(nodeName(e.to))}</span></div>`).join('')||'<p class="note">책임자와 목표를 연결하면 그래프가 만들어집니다.</p>'}</section>${diagnosticPanel()}`;}
function nodeName(n){return n.tag==='PersonNode'?person(n.contents):n.tag==='GoalNode'?state.goals.find(x=>x.goal.id===n.contents)?.goal.description||n.contents:n.contents;}
function authority(){return `<section class="panel"><h2>충분한 권한이 책임을 가능하게 합니다</h2><p class="muted">권한 축소로 활성 목표의 요건이 깨지면 해당 목표는 자동으로 초안으로 돌아갑니다.</p><p class="note">집중도 = 보유 권한 종류 수 + 예산 보유 1점 / 조직 전체 점수. 실제 의사결정 빈도나 권력의 측정값은 아닙니다.</p></section><div class="grid">${state.people.map(p=>{const a=auth(p.id),ps=[...(a?.canApprove||[]),...(a?.canHire?['Hiring']:[]),...(a?.canChangePrice?['Pricing']:[])];return `<section class="panel" id="authority-${esc(p.id)}"><span class="tag">권한 비중 ${Math.round((state.decisionShare[p.id]||0)*100)}%</span><h2>${esc(p.name)}</h2><p class="muted">${esc(p.role)}</p><form data-form="authority" data-id="${esc(p.id)}">${input('집행 가능한 예산 한도 (KRW)','budget','number',a?.budgetLimit||0)}${checks(ps)}<button>권한 저장</button></form><p class="note">담당 목표 ${state.goals.filter(x=>x.owner===p.id).length}개</p></section>`;}).join('')||'<section class="panel empty">Dashboard에서 구성원을 먼저 추가하세요.</section>'}</div>${diagnosticPanel()}`;}
function reviews(){return `<details class="panel" open><summary>+ 회고와 다음 결정 기록</summary><form data-form="review" id="review-form">${select('회고할 목표','goal',goalsOptions())}${input('회고 요약','note')}<label>새롭게 배운 점<textarea name="learning" placeholder="결과를 통해 무엇을 배웠나요?"></textarea></label>${input('다음 결정 (선택)','decision','text','',false)}<div class="fields"><label>결정 담당자<select name="decisionOwner">${personOptions()}</select></label>${input('결정 기한 (선택)','decisionDeadline','date','',false)}</div><p class="note">현재 최신 결과와 평가가 함께 보존됩니다. 결정과 학습이 모두 없으면 컴파일러가 경고합니다.</p><button>회고 기록</button></form></details><div class="grid">${state.reviews.map(r=>`<section class="panel"><span class="tag">${date(r.heldAt)} · ${statusNames[r.evaluation.status]}</span><h2>${esc(state.goals.find(x=>x.goal.id===r.goal)?.goal.description)}</h2><p>${esc(r.note)}</p><h3>Learning</h3>${r.learnings.map(l=>`<p>${esc(l.text)}</p>`).join('')||'<p class="muted">기록된 학습 없음</p>'}<h3>Decision</h3>${r.decisions.map(d=>`<p>${esc(d.text)}<br><small>${esc(person(d.owner))} · ${d.deadline?date(d.deadline):'기한 미정'}</small></p>`).join('')||'<p class="muted">기록된 결정 없음</p>'}${(state.reviewWarnings.find(w=>w.id===r.id)?.warnings||[]).map(w=>`<p class="tag warn">${esc(w)}</p>`).join('')}</section>`).join('')}</div><section class="panel" id="audit-history"><h2>조직의 의사결정 기록</h2><p class="note">서버가 시각과 순번을 부여합니다. 행위자는 요청의 기록 주체이며 인증된 신원 증명이 아닙니다.</p>${state.events.map(e=>`<div class="event"><small>#${e.record.seq} · ${new Date(e.record.at).toLocaleString('ko-KR')}<br>${esc(e.record.actor?person(e.record.actor):'로컬 운영자 (미인증)')}</small><p>${esc(e.description)}</p></div>`).join('')||'<p class="muted">아직 기록이 없습니다.</p>'}</section>`;}
content.addEventListener('submit',async event=>{
 event.preventDefault();const form=event.target,data=new FormData(form),get=n=>data.get(n),kind=form.dataset.form,id=form.dataset.id,orgId=form.dataset.org||null,context=Number(form.dataset.context);let path,body;
 if(!currentContext(orgId,context))return;
 if(kind==='rename'&&!stateIsFresh){notify('최신 정보를 불러오지 못했습니다. 새로고침 후 다시 시도해 주세요.',true);return;}
 try{switch(kind){case 'rename':path=orgPath(orgId);body={name:get('name'),expectedVersion:Number(form.dataset.version)};break;case 'organization':path='organizations';body={id:uid('org'),name:get('name')};break;case 'person':path='people';body={id:uid('person'),name:get('name'),role:get('role')};break;case 'goal':path='goals';body={id:uid('goal'),organization:orgId,description:get('description'),metric:{id:get('metricId'),name:get('metricName'),unit:get('unit'),direction:get('direction')},baseline:Number(get('baseline')),target:Number(get('target')),startsAt:new Date(get('startsAt')).toISOString(),deadline:new Date(get('deadline')).toISOString(),parent:get('parent')||null,requiredPermissions:data.getAll('permissions'),requiredBudget:Number(get('budget'))};break;case 'owner':path=`goals/${encodeURIComponent(id)}/owner`;body={owner:get('owner')};break;case 'authority':path=`people/${encodeURIComponent(id)}/authority`;body={owner:id,budgetLimit:Number(get('budget')),canHire:false,canChangePrice:false,canApprove:data.getAll('permissions')};break;case 'result':path=`goals/${encodeURIComponent(id)}/results`;body={value:Number(get('value')),reportedBy:get('reportedBy'),note:get('note'),actor:get('reportedBy')};break;case 'strategy':path=`goals/${encodeURIComponent(id)}/strategy`;body={note:get('note')};break;case 'review':path='reviews';if(get('decision')&&!get('decisionOwner'))throw Error('결정 담당자를 선택하세요.');body={id:uid('review'),goal:get('goal'),note:get('note'),learnings:get('learning').trim()?[{text:get('learning')}]:[],decisions:get('decision').trim()?[{text:get('decision'),owner:get('decisionOwner'),deadline:get('decisionDeadline')?new Date(get('decisionDeadline')+'T23:59:59').toISOString():null}]:[]};break;default:return;}
  if(!['organization','rename'].includes(kind))path=orgPath(orgId,path);
  form.querySelectorAll('button').forEach(b=>b.disabled=true);
  await api(path,body,kind==='rename'?'PATCH':'POST');
  if(!currentContext(orgId,context))return;
  notify(kind==='organization'?'조직을 등록했습니다. 목록에서 조직을 열어 구성원과 목표를 추가하세요.':'저장했습니다. 조직 상태와 감사 기록을 갱신했습니다.');await refresh();
 }catch(e){if(!currentContext(orgId,context))return;if(kind==='rename'&&e.status===409){const refreshed=await refresh();if(!currentContext(orgId,context))return;notify(e.message+' 자동 재시도하지 않았습니다. '+(refreshed?'새로고침된 이름을 확인하고 다시 입력해 주세요.':'최신 정보를 불러오지 못했습니다. 새로고침 후 다시 시도해 주세요.'),true);}else notify(e.message,true);form.querySelectorAll('button').forEach(b=>b.disabled=kind==='rename'&&!stateIsFresh);}
});
document.addEventListener('click',async event=>{
 const b=event.target.closest('button');if(!b)return;
 if(b.dataset.view){await navigate(b.dataset.view,b.dataset.view==='organizations'?null:selectedOrg);return;}
 if(b.id==='refresh'){dismissConfirmation();await refresh();return;}
 if(b.id==='organization-settings'){await navigate('organization',selectedOrg);return;}
 if(b.id==='open-organization-delete'){openDeletion();return;}
 if(b.id==='cancel-organization-delete'){deletionDialog.close();return;}
 if(b.dataset.guide){navigateGuide(b);return;}
 if(b.dataset.action==='open-org'||b.dataset.action==='detail-org'){await navigate(b.dataset.action==='open-org'?'dashboard':'organization',b.dataset.id);return;}
 if(!b.dataset.action)return;
 const id=selectedOrg,context=contextToken;b.disabled=true;
 try{
  if(b.dataset.action==='demo')await api('demo',{});
  else if(b.dataset.action==='activate')await api(orgPath(id,`goals/${encodeURIComponent(b.dataset.id)}/activate`),{});
  else if(b.dataset.action==='evaluate')await api(orgPath(id,'evaluations'),{goal:b.dataset.id});
  if(!currentContext(id,context))return;
  notify('변경 내용을 기록했습니다.');await refresh();
 }catch(e){if(!currentContext(id,context))return;await refresh();if(!currentContext(id,context))return;notify(e.message,true);b.disabled=false;}
});

function resultHistory(x){return x.results.length?`<div class="result-history"><h3>결과 추이 · 최근 순</h3><table><thead><tr><th>기록 시각</th><th>측정값</th><th>설명</th></tr></thead><tbody>${x.results.map(r=>`<tr><td>${new Date(r.reportedAt).toLocaleString('ko-KR')}</td><td>${number(r.value)} ${esc(x.goal.metric.unit)}</td><td>${esc(r.note)}</td></tr>`).join('')}</tbody></table></div>`:'';}
function demoGuide(){
 const get=id=>state.goals.find(x=>x.goal.id==='demo-'+id);
 const partner=get('partners'),launch=get('launch'),revenue=get('revenue');
 const assigned=Boolean(partner?.owner),ready=launch?.analysis.coverage===1;
 const evaluated=state.events.some(e=>e.record.event.tag==='GoalEvaluated'&&e.record.event.contents?.[0]==='demo-revenue'&&e.record.event.contents?.[1]?.status==='Achieved');
 const achieved=revenue?.evaluation.status==='Achieved';
 const reviewed=state.reviews.some(r=>r.goal==='demo-revenue'&&r.evaluation.status==='Achieved'&&r.learnings.length>0&&r.decisions.some(d=>d.owner&&d.deadline));
 const steps=[
  {done:Boolean(partner?.active),title:'01 · 빈 책임 자리 채우기',text:partner?.active?'파트너십 목표에 책임자가 지정되고 활성화됐습니다. 다음은 제품 책임자의 권한을 보완할 차례입니다.':assigned?'책임자가 지정됐습니다. 해당 목표를 활성화하세요.':'파트너십 목표에 한유진을 최종 책임자로 지정하세요. 이미 계약 권한을 가진 구성원입니다.',view:assigned?'dashboard':'responsibility',target:assigned?'goal-demo-partners':'owner-demo-partners',action:assigned?'목표 활성화하러 가기':'책임자 지정하러 가기'},
  {done:Boolean(launch?.active),title:'02 · 책임에 맞는 권한 주기',text:launch?.active?'신제품 출시 목표가 충분한 권한과 함께 활성화됐습니다. 다음은 매출 결과를 보고하고 평가를 남겨보세요.':ready?'필요한 권한과 예산을 갖췄습니다. 신제품 출시 목표를 활성화하세요.':'제품 책임자 이지원에게 채용 권한과 예산 30,000,000원을 부여하세요. 기존 제품 출시 권한도 유지합니다.',view:ready?'dashboard':'authority',target:ready?'goal-demo-launch':'authority-demo-product',action:ready?'목표 활성화하러 가기':'권한 편집하러 가기'},
  {done:Boolean(achieved&&evaluated),title:'03 · 결과에서 평가까지',text:achieved&&evaluated?'매출 달성 결과와 평가가 모두 기록됐습니다. 이제 회고에서 배움과 다음 결정을 남겨보세요.':achieved?'매출이 달성 상태입니다. 별도의 평가 기록 버튼으로 평가를 남기세요.':'매출 목표의 실측값을 50억원으로 보고하고 평가 기록을 누르세요. 결과 설명과 보고자도 입력합니다.',view:'dashboard',target:'result-demo-revenue',action:achieved?'평가 기록하러 가기':'결과 보고하러 가기'},
  {done:reviewed,title:'04 · 배움을 다음 결정으로',text:reviewed?'달성 결과와 학습, 담당자·기한 있는 다음 결정이 회고에 보존됐습니다. 그래프와 감사 기록에서 운영 흐름을 다시 살펴보세요.':'매출 목표를 선택해 학습과 다음 결정, 담당자, 미래 기한을 기록하세요. 달성 결과와 평가가 회고에 함께 보존됩니다.',view:'reviews',target:'review-form',action:'회고 작성하러 가기'}
 ];
 const count=steps.filter(x=>x.done).length;
 return `<section class="demo-guide"><div class="demo-heading"><div><span class="tag">DEMO WORKSPACE · 가상 데이터</span><h2>조직의 운영 흐름, 네 단계로 체험하세요</h2><p>6명 · 7개 목표 · 5가지 성과 상태. 다른 조직과 독립적으로 관리되는 가상 조직입니다.</p></div><span class="guide-count">${count} / 4 완료</span></div><details id="demo-guide-details" ${guideOpen?'open':''}><summary>체험 가이드 ${guideOpen?'접기':'열기'} · 실제 저장 상태로 진행률을 계산합니다</summary><div class="guide-steps">${steps.map(step=>`<article class="guide-step ${step.done?'complete':''}"><span class="step-state">${step.done?'✓ 완료':'○ 체험 대기'}</span><h3>${step.title}</h3><p>${step.text}</p><button class="secondary" data-guide="${step.view}" data-target="${step.target}">${step.done?'다시 살펴보기':step.action} →</button></article>`).join('')}</div><p class="note">전사 성장 지수는 하위 목표의 자동 합계가 아닌 별도 보고 KPI입니다. 초기 O001·O017·O031·O040은 의도한 체험 사례입니다. 권한 부족 목표를 먼저 활성화해 오류를 확인해도 됩니다. 결과 추이의 초기 값은 가상 연속 샘플이며 감사 시각은 실제 가져온 시각입니다.</p><div class="actions"><button class="secondary" data-guide="responsibility" data-target="responsibility-graph">관계 그래프 살펴보기 →</button><button class="secondary" data-guide="reviews" data-target="audit-history">샘플 회고와 감사 기록 살펴보기 →</button></div></details></section>`;
}
function navigateGuide(button){
 dismissConfirmation();contextToken++;requestToken++;view=button.dataset.guide;render();
 const target=button.dataset.target&&document.getElementById(button.dataset.target);
 if(target){let parent=target;while(parent){if(parent.tagName==='DETAILS')parent.open=true;parent=parent.parentElement;}
   target.querySelectorAll('details').forEach(d=>d.open=true);
   if(target.id==='review-form')target.querySelector('[name="goal"]').value='demo-revenue';
   target.scrollIntoView({behavior:'smooth',block:'center'});
   const focus=target.querySelector('input:not([type="checkbox"]),select,textarea');if(focus)focus.focus({preventScroll:true});
 }else document.querySelector('#title').scrollIntoView({behavior:'smooth'});
}
content.addEventListener('toggle',event=>{if(event.target.id==='demo-guide-details')guideOpen=event.target.open;},true);

refresh();

function organizationSettings(){return `<section class="panel organization-settings" id="organization-settings-panel"><div><span class="eyebrow">WORKSPACE SETTINGS</span><h2>조직 설정</h2><p><strong>${esc(state.organization.name)}</strong></p><p class="muted">구성원, 목표, 책임, 권한, 결과, 평가, 회고와 전략을 현재 워크스페이스에서 제거합니다.</p><p class="note">원본 감사 이벤트는 파일·DB에 보존되는 논리 삭제입니다. 삭제 후 새 조직이나 새 데모를 만들 수 있습니다.</p></div><button type="button" class="danger-outline" id="open-organization-delete">조직 삭제…</button></section>`;}
function openDeletion(){
 if(!state?.organization)return;
 if(!stateIsFresh){notify('최신 상태를 불러오지 못했습니다. 새로고침한 뒤 삭제 확인창을 다시 열어주세요.',true);return;}
 deletionSnapshot={id:state.organization.id,name:state.organization.name,version:state.version};
 deleting=false;
 document.querySelector('#delete-organization-name').textContent=deletionSnapshot.name;
 document.querySelector('#delete-organization-confirm').value='';
 document.querySelector('#delete-organization-error').textContent='';
 document.querySelector('#confirm-organization-delete').disabled=true;
 document.querySelector('#cancel-organization-delete').disabled=false;
 document.querySelector('#delete-organization-dialog').showModal();
 document.querySelector('#delete-organization-confirm').focus();
}
const deletionDialog=document.querySelector('#delete-organization-dialog');
deletionDialog.addEventListener('cancel',event=>{if(deleting)event.preventDefault();});
deletionDialog.addEventListener('close',()=>{deletionSnapshot=null;document.querySelector('#delete-organization-name').textContent='';document.querySelector('#delete-organization-confirm').value='';});
document.querySelector('#delete-organization-confirm').addEventListener('input',event=>{
 document.querySelector('#confirm-organization-delete').disabled=deleting||!deletionSnapshot||event.target.value!==deletionSnapshot.name;
});
document.querySelector('#delete-organization-form').addEventListener('submit',async event=>{
 event.preventDefault();
 const snapshot=deletionSnapshot,context=contextToken;
 const confirmName=document.querySelector('#delete-organization-confirm').value;
 if(deleting||!snapshot||confirmName!==snapshot.name)return;
 deleting=true;
 document.querySelector('#confirm-organization-delete').disabled=true;
 document.querySelector('#cancel-organization-delete').disabled=true;
 try{
   await api(orgPath(snapshot.id),{confirmName,expectedVersion:snapshot.version},'DELETE');
   if(!currentContext(snapshot.id,context))return;
   deletionDialog.close();
   // Clear the deleted workspace before attempting any further reads.
   organizations=organizations.filter(x=>x.organization.id!==snapshot.id);
   const nextContext=contextToken+1;
   await navigate('organizations');
   if(!currentContext(null,nextContext))return;
   notify(stateIsFresh?'조직을 논리 삭제했습니다. 다른 조직과 원본 감사 기록은 보존됩니다.':'조직 삭제는 완료됐지만 최신 목록 조회에 실패했습니다. 이전 조직은 화면에서 제거했습니다. 새로고침해 주세요.',!stateIsFresh);
 }catch(error){
   if(!currentContext(snapshot.id,context))return;
   deletionDialog.close();const refreshed=await refresh();
   if(!currentContext(snapshot.id,context))return;
   notify(error.message+(error.status===409?' 자동 재시도하지 않았습니다.':'')+(refreshed?' 최신 상태를 확인하고 삭제 확인창을 다시 열어주세요.':' 최신 상태 조회에도 실패했습니다. 새로고침에 성공한 뒤 다시 확인하세요.'),true);
 }finally{deleting=false;}
});
