sap.ui.define([
    "sap/m/MessageToast", "sap/m/BusyDialog", "sap/m/MessageBox", "sap/m/PDFViewer"
], function(MessageToast, BusyDialog, MessageBox, PDFViewer) {
    'use strict';

    return {
        send_mail: function(oEvent) {
            const oBusyDialog = new BusyDialog();
            oBusyDialog.open();
            var aBatchOperations = [];
            var contextTable = this.getSelectedContexts("ZFI007_R_MUS_SAT_MUTABAKAT");
            var oModel = this.getModel();
            contextTable.forEach(function (row) {
                var aPath = row["sPath"] + "/com.sap.gateway.srvd.zfi007_ui_mus_sat_mutabakat.v0001.send_mail(...)";
                var oAction = oModel.bindContext(aPath, null, {
                    $$groupId: "send_mail"
                });

                oAction.setParameter("CUSTOMER_NAME", row.getObject().customer_name);
                oAction.setParameter("SUPPLIER_NAME", row.getObject().supplier_name);
                oAction.setParameter("COMPANY_CODE", row.getObject().company_Code);
                oAction.setParameter("DEBIT_AMOUNT", row.getObject().debit_amount);
                oAction.setParameter("CREDIT_AMOUNT", row.getObject().credit_amount);
                oAction.setParameter("TAX_NUMBER1", row.getObject().tax_number1);
                oAction.setParameter("TAX_NUMBER2", row.getObject().tax_number2);
                oAction.setParameter("TELEPHONE_NUMBER1", row.getObject().telephone_number1);
                oAction.setParameter("FAX_NUMBER", row.getObject().fax_number);
                oAction.setParameter("CURRENCY", row.getObject().currency);

                oAction.execute();

                aBatchOperations.push(oAction);
            }
            )

            oModel.submitBatch("send_mail").then(function (data) {
                var row = aBatchOperations[0];

                if (sap.ui.getCore().getMessageManager().getMessageModel().getData().some(oMessage => oMessage.type === "Error")) {
                    MessageBox.alert(row.getBoundContext().getModel().mMessages[""][0].message, {
                        icon: MessageBox.Icon.ERROR,
                        title: "Error"
                    });
                }
                else {
                    //aBatchOperations.forEach(function (row) {
                    if (row.getBoundContext().getObject().value) {
                        var vBase64 = row.getBoundContext().getObject().value;
                        let oPDFViewer = new PDFViewer({
                            isTrustedSource: true,
                            showDownloadButton: false
                        });
                        jQuery.sap.addUrlWhitelist("data");
                        if (vBase64) {
                            oPDFViewer.setSource("data:application/pdf;base64," + vBase64);
                        }
                        oPDFViewer.attachEventOnce("sourceValidationFailed", (oEventSVF) => {
                            oEventSVF.preventDefault();
                        });
                        oPDFViewer.setTitle("PDF");
                        oPDFViewer.open();

                    }
                    //})
                }
                oBusyDialog.close();
                sap.ui.getCore().getMessageManager().removeAllMessages();

            }.bind(this),
                function (oError) {
                    MessageBox.alert(oError.message, {
                        icon: MessageBox.Icon.ERROR,
                        title: "Error"
                    });
                    oBusyDialog.close();
                });

        }
    };
});
