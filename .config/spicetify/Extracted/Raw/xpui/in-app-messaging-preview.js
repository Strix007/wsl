"use strict";(("undefined"!=typeof self?self:global).webpackChunkclient_web=("undefined"!=typeof self?self:global).webpackChunkclient_web||[]).push([[924],{98454:(e,t,n)=>{n.r(t),n.d(t,{InAppMessagingPreviewTool:()=>m,default:()=>g});var l=n(67294),a=n.n(l),s=n(65858),i=n(5965),p=n(79145),r=n(62890);const o="UWu01mES0C8oeXPEpkMa",d="F_d1bU5PZiXeyp15OOvs",c="YN0EidK_DRUB0K4T9hVs",u="vlsm1_I1jd_w1AU60HHT",v=[{label:"Production",value:"prod"},{label:"Development",value:"dev"}],m=()=>{const e=(0,s.I0)(),t=(0,s.v9)((e=>e.inAppMessaging.env)),n=(0,s.v9)((e=>e.inAppMessaging.message)),[m,g]=(0,l.useState)(""),[E,f]=(0,l.useState)(""),[A,b]=(0,l.useState)(!1);(0,l.useEffect)((()=>{n&&A&&b(!1)}),[n,A]);const k=!n&&A;return a().createElement("div",{className:o},a().createElement("h1",null,"In-App Messaging Preview"),a().createElement("div",{className:d},a().createElement("div",null,a().createElement("label",{htmlFor:"desktop.settings.inAppMessagingEnv"},"Mode (endpoint):")),a().createElement(p.v,{dir:"auto",value:t,id:"desktop.settings.inAppMessagingEnv",onSelect:function(t){"prod"!==t&&"dev"!==t||e((0,i.q1)(t))}},v.map((({label:e,value:t})=>a().createElement("option",{key:t,value:t},e)))),a().createElement("div",null,a().createElement("label",{htmlFor:"desktop.settings.inAppMessagingCreativeId"},"Creative Id:")),a().createElement("div",null,a().createElement("input",{className:c,placeholder:"(eg. 12123434)",id:"desktop.settings.inAppMessagingCreativeId",name:"Creative Id",type:"text",value:m,onChange:function(e){g(e.target.value),b(!1)}})),a().createElement("div",null,a().createElement("label",{htmlFor:"desktop.settings.inAppMessagingAppUri"},"URI for Dynamic Content (optional):")),a().createElement("div",null,a().createElement("input",{className:c,placeholder:"(eg. spotify:artist:123)",id:"desktop.settings.inAppMessagingAppUri",name:"App Uri",type:"text",value:E,onChange:function(e){f(e.target.value),b(!1)}})),a().createElement("div",null),a().createElement("div",null),a().createElement("div",null,a().createElement(r.z,{version:"secondary",onClick:function(t){t.preventDefault(),e((0,i.XJ)(m,E)),b(!1),window.setTimeout((()=>{b(!0)}),1e3)}},"Get Preview")),k&&a().createElement("div",{className:u},"No Messsage found")))},g=m}}]);
//# sourceMappingURL=in-app-messaging-preview.js.map