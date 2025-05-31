@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Domain Read'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.resultSet.sizeCategory: #XS
define view entity ZFI007_I_REPORT_TYPE_VH
  as select distinct from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name: 'ZFI007_D_REPORT_TYPE')
{
  key value_low,
      text
}
where
  language = 'E'
