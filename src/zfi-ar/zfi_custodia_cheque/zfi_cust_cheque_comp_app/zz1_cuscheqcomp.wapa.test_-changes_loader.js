const connectorPath=parseFloat(sap.ui.version)>=1.8?"sap/ui/fl/write/api/connectors/FileListBaseConnector":"sap/ui/fl/initial/api/connectors/FileListBaseConnector";sap.ui.define(["sap/base/util/merge",connectorPath],function(n,e){var a=[];var o=[/^localh+
ost$/,/^.*.applicationstudio.cloud.sap$/];var i=new URL(window.location.toString());var t=o.some(n=>n.test(i.hostname));return n({},e,{getFileList:function(){return new Promise(function(n,e){if(!t)e(console.log("cannot load flex changes: invalid host"));+
$.ajax({url:i.origin+"/changes/",type:"GET",cache:false}).then(function(e){var a=/(\/changes\/[^"]*\.[a-zA-Z]*)/g;var o=a.exec(e);var i=[];while(o!==null){i.push(o[1]);o=a.exec(e)}n(i)}).fail(function(e){n()})})}})});                                      
//# sourceMappingURL=changes_loader.js.map                                                                                                                                                                                                                     