package org.kalypso.ogc.gml.test;

import junit.framework.TestCase;

import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.util.xml.XMLTools;

/**
 * @author sbad0205
 */
public class KalypsoFeatureTest extends TestCase
{

  private KalypsoFeature createFeature()
  {
  try
  {
    final GMLSchema schema = new GMLSchema(
      XMLTools.getAsDOM(
          KalypsoFeatureTest.class.getResourceAsStream("point.xsd")));
    final FeatureType featureType = schema.getFeatureTypes()[0];
    
    final Object[] properties = new Object[featureType.getProperties().length];
    return new KalypsoFeature(FeatureFactory.createFeature( "x", featureType, properties ));
 }
  catch( Exception e )
  {
    e.printStackTrace();
  }
 return null;
  }
  public void testSelect()
  {
    KalypsoFeature fe=createFeature();
    int s1=10;
    fe.select(s1);
    assertEquals(true,fe.isSelected(s1));
   
  }

  public void testUnselect()
  {
    KalypsoFeature fe=createFeature();
    int s1=10;
    fe.select(s1);
    assertEquals(true,fe.isSelected(s1));
    fe.unselect(s1);
    assertEquals(false,fe.isSelected(s1));  
  }

  public void testToggle()
  {
    KalypsoFeature fe=createFeature();
    int s1=10;
    fe.toggle(s1);
    assertEquals(true,fe.isSelected(s1));
    fe.toggle(s1);
    assertEquals(false,fe.isSelected(s1));
  }

  
}
