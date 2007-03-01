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
	  public final static QName WB1D2D_F_NODE = 
            new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Node" );

      public final static QName WB1D2D_PROP_POINT = 
             new QName( NS.GML3, "pointProperty" );
      
      public final static QName WB1D2D_F_EDGE = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Edge" );
      
      public final static QName WB1D2D_F_EDGE_INV = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "EdgeInv" );

      public final static QName WB1D2D_PROP_DIRECTEDNODE = 
                            new QName( 
                                    UrlCatalog1D2D.MODEL_1D2D_NS, 
                                    "directedNode" );
      public final static QName WB1D2D_PROP_EDGE_CONTAINERS=
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edgeContainer");
    
      public final static QName WB1D2D_PROP_ELEMENT_CONTAINERS=
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "elementContainer");
    
      
      public final static QName WB1D2D_PROP_NODE_CONTAINERS=
          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "nodeContainer" );
      
      public final static QName WB1D2D_F_ELEMENT = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element" );
      
      public final static QName WB1D2D_F_ELEMENT1D = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element1D" );

      public final static QName WB1D2D_F_FE1D2D_2DElement = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "Element2D" );

      public final static QName WB1D2D_F_FE1D2DContinuityLine = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ContinuityLine" );
      
      public static final QName WB1D2D_F_POLY_ELEMENT = 
            new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "PolyElement" );

      public final static QName WB1D2D_PROP_DIRECTEDEDGE = 
          new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "directedEdge" );
      
      public final static QName WB1D2D_PROP_FE1D2D_MIDDLE_NODE = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS,"wb1d2d:middleNode");
        
      public final static QName WB1D2D_PROP_FE1D2DEDGE_CONTAINER= 
        new QName(UrlCatalog1D2D.MODEL_1D2D_NS, "wb1d2d:edgeContainer");
      
      
      public static final QName WB1D2D_F_COMPLEX_ELE = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ComplexElement" );
      
      public static final QName WB1D2D_F_COMPLEX_ELE_2D = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "ComplexElement2D" );
      
      public static final QName WB1D2D_F_MAIN_RIVER_CHANNEL = 
        new QName( 
              UrlCatalog1D2D.MODEL_1D2D_NS, 
              "MainChannel" );
      
      public final static QName WB1D2D_PROP_COMPLEX_ELE_CONTAINER= 
        new QName(
                  UrlCatalog1D2D.MODEL_1D2D_NS, 
                  "wb1d2d:complexElementContainer");
      
      public final static QName WB1D2D_PROP_ELE_2D= 
        new QName(
                  UrlCatalog1D2D.MODEL_1D2D_NS, 
                  "wb1d2d:element2D");
      //fe1d2d_2DElement
      /////////////////////////////////////////////////////////////////
      public final static QName WB1D2D_F_DiscretisationModel = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "DiscretisationModel" );

      /**
       * QName for a property linking a feature to an edge.
       * Use for example in DiscretisationModel feature
       */
      public final static QName WB1D2D_PROP_EDGES = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" );
    
      public final static QName WB1D2D_PROP_EDGE_IN_INV = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edge" );
      
      public final static QName WB1D2D_PROP_EDGEINV = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "edgeInv" );
      
      /**
       * QName for a property linking a feature to an element.
       * Use for example in DiscretisationModel feature
       */
      public final static QName WB1D2D_PROP_ELEMENTS = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element" );
    
      /**
       * QName for a property linking a feature to a node.
       * Use for example in DiscretisationModel feature
       */
      public final static QName WB1D2D_PROP_NODES = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "node" );
      
      /**
       * QName for a property linking a feature to a node.
       * Use for example in DiscretisationModel feature
       */
      public final static QName WB1D2D_PROP_COMPLEX_ELEMENTS = 
              new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "complexElement" );
      
      
      public static final QName WB1D2D_PROP_EDGE_GEOM = 
//        new QName( NS.GML3, "curveProperty" );
               new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" );
      public static final QName WB1D2D_PROP_ELEMENT_GEOM = 
        new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "geometry" );
}
