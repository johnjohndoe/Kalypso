package org.kalypso.kalypsosimulationmodel.schema;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;

public class KalypsoModelRoughnessConsts
{
  public static final QName WBR_PROP_URI = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "uri" );

  public static final QName WBR_F_ROUGHNESS = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessCls" );

  public static final QName WBR_PROP_KS = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "ks" );

  public static final QName WBR_PROP_AXAY = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "axay" );

  public static final QName WBR_PROP_DP = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "dp" );

  public static final QName WBR_PROP_EDDY = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "eddy" );

  public static final QName WBR_PROP_CHARACTV = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "charactV" );

  // //corrections
  public static final QName WBR_F_ROUGHNESS_CORRECTION = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessClsCorrection" );

  public static final QName WBR_PROP_KS_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "ksCor" );

  public static final QName WBR_PROP_AXAY_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "axayCor" );

  public static final QName WBR_PROP_DP_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "dpCor" );

  public static final QName WBR_PROP_EDDY_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "eddyCor" );

  public static final QName WBR_PROP_MARSH_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "marshCor" );

  // roughness collection
  public static final QName WBR_F_ROUGHNESS_CLS_COLLECTION = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessClsCollection" );

  public static final QName WBR_PROP_ROUGHNESS_CLS_MEMBER = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughnessClsMember" );
//  colorStyle
  public static final QName WBR_PROP_COLOR_STYLE = 
          new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "colorStyle" );
  // gml
  public static final QName GML_PROP_ID = new QName( NS.GML3, "id" );

  public static final QName GML_PROP_NAME = new QName( NS.GML3, "name" );

  public static final QName GML_PROP_DESCRIPTION = new QName( NS.GML3, "description" );
}
