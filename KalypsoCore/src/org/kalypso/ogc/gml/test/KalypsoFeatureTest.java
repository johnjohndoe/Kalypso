package org.kalypso.ogc.gml.test;

import junit.framework.TestCase;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author sbad0205
 */
public class KalypsoFeatureTest extends TestCase
{
  private Feature createFeature()
  {
  try
  {
    final GMLSchema schema = new GMLSchema(
          KalypsoFeatureTest.class.getResource("point.xsd"));
    final FeatureType featureType = schema.getFeatureTypes()[0];
    
    return 
    FeatureFactory.createFeature("x",featureType);
 }
  catch( Exception e )
  {
    e.printStackTrace();
  }
 return null;
  }
  public void testSelect()
  {
    Feature fe=createFeature();
    int s1=10;
    fe.select(s1);
    assertEquals(true,fe.isSelected(s1));
   
  }

  public void testUnselect()
  {
    Feature fe=createFeature();
    int s1=10;
    fe.select(s1);
    assertEquals(true,fe.isSelected(s1));
    fe.unselect(s1);
    assertEquals(false,fe.isSelected(s1));  
  }

  public void testToggle()
  {
    Feature fe=createFeature();
    int s1=10;
    fe.toggle(s1);
    assertEquals(true,fe.isSelected(s1));
    fe.toggle(s1);
    assertEquals(false,fe.isSelected(s1));
  }

  
}
