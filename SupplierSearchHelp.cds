@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FI008 - Supplier VH'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZFI007_I_SUPPLIER_VH 
as select from I_Supplier

{
    key Supplier,
    SupplierName
}
