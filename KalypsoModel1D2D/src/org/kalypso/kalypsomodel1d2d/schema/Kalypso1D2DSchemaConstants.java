package org.kalypso.kalypsomodel1d2d.schema;

import javax.xml.namespace.QName;

/**
 * @author Patrice Congo
 */
public class Kalypso1D2DSchemaConstants
{
  public final static QName WB1D2D_F_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Node" ); //$NON-NLS-1$

  public final static QName WB1D2D_F_MIDDLE_NODE = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "MiddleNode" ); //$NON-NLS-1$

  /**
   * @deprecated
   */
  @Deprecated
  public final static QName WB1D2D_F_EDGE_INV = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeInv" ); //$NON-NLS-1$

  /**
   * @deprecated
   */
  @Deprecated
  public final static QName WB1D2D_PROP_EDGE_IN_INV = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" ); //$NON-NLS-1$

  /**
   * @deprecated
   */
  @Deprecated
  public final static QName WB1D2D_PROP_EDGEINV = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edgeInv" ); //$NON-NLS-1$

  public static final QName WB1D2D_PROP_EDGE_GEOM = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" ); //$NON-NLS-1$

  public static final QName WB1D2D_PROP_ELEMENT_GEOM = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" ); //$NON-NLS-1$

  public static final QName OP1D2D_PROP_PARENT_CALCUNIT = new QName( UrlCatalog1D2D.MODEL_1D2DOperational_NS, "parentCalculationUnit" ); //$NON-NLS-1$

  public static final QName TIN_RESULT = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "TinResult" ); //$NON-NLS-1$

  public static final QName TIN_RESULT_PROP_TIN = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "triangulatedSurfaceMember" ); //$NON-NLS-1$

  public static final QName TIN_RESULT_PROP_PARAMETER = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ); //$NON-NLS-1$

  public static final QName TIN_RESULT_PROP_UNIT = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ); //$NON-NLS-1$

  public static final QName NODE_RESULT = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "NodeResult" ); //$NON-NLS-1$

  public static final QName NODE_RESULT_PROP_MEMBER = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "nodeResultMember" ); //$NON-NLS-1$
}
