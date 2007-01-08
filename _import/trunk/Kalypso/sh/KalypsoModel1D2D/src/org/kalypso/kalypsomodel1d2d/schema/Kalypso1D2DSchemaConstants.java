package org.kalypso.kalypsomodel1d2d.schema;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;

/**
 * @author Patrice Congo
 */
public class Kalypso1D2DSchemaConstants
{
	
	public static final QName SIMULATION_MODEL1D2D=
			new QName(UrlCatalog1D2D.MODEL_1D2D_NS,"SimulationModelType1D2D");
////fem
	  public final static QName WB1D2D_F_FE1D2DNODE = 
            new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DNode" );

      public final static QName WB1D2D_PROP_POINT = 
             new QName( NS.GML3, "pointProperty" );
      
      public final static QName WB1D2D_F_FE1D2DEDGE = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DEdge" );

      public final static QName WB1D2D_PROP_DIRECTEDNODE = 
                            new QName( 
                                    UrlCatalog1D2D.MODEL_1D2D_NS, 
                                    "fe1d2dDirectedNode" );
      public final static QName WB1D2D_PROP_EDGE_CONTAINERS=
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dEdgeContainer");
    
      public final static QName WB1D2D_PROP_ELEMENT_CONTAINERS=
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dElementContainer");
    
      
      public final static QName WB1D2D_PROP_NODE_CONTAINERS=
          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dNodeContainer" );
      
      public final static QName WB1D2D_F_FE1D2D_2DElement = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2D_2DElement" );

      public static final QName WB1D2D_F_FE1D2DTriElement = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DTriElement" );

      public static final QName WB1D2D_MODEL1D2D_F_FE1D2DQuadriElement = 
            new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "FE1D2DQuadriElement" );

      public final static QName WB1D2D_PROP_DIRECTEDEDGE = 
          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "fe1d2dDirectedEdge" );
      public final static QName WB1D2D_PROP_FE1D2D_MIDDLE_NODE = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS,"wb1d2d:fe1d2dMiddleNode");
        
      public final static QName WB1D2D_PROP_FE1D2DEDGE_CONTAINER= 
        new QName(UrlCatalog1D2D.MODEL_1D2D_NS, "wb1d2d:fe1d2dEdgeContainer");
      
}
