package org.kalypso.ogc.gml.expressions;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.runtime.Assert;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypsodeegree.model.feature.Feature;

public class QNamePropertyTester extends PropertyTester
{
  public boolean test( final Object receiver, final String property, final Object[] args, final Object expectedValue )
  {
    if( "qname".equals( property ) )
    {
      final Feature feature = (Feature) receiver;
      final String excpectedStr = expectedValue.toString().replaceAll( "\"", "" ); // strip "
      final QName expectedQName = QName.valueOf( excpectedStr );
      return GMLSchemaUtilities.substitutes( feature.getFeatureType(), expectedQName );
    }

    Assert.isTrue( false );
    return false;
  }

}
