@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'FI008 - Customer VH'
@Metadata.ignorePropagatedAnnotations: true
//@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZFI007_I_CUSTOMER_VH 
as select from I_Customer

{
    key Customer,
    CustomerName
}
