package org.kalypsodeegree_impl.gml.binding.math;

import javax.xml.namespace.QName;

import org.kalypsodeegree.model.feature.Feature;

/**
 * Exception thrown whenever to internal configuaration of a feature in illegal: e.g. missing properties, properties
 * with the bad type
 * 
 * @author Patrice Congo
 */
public class IllegalFeatureState extends Exception
{
  /**
   * the feature which state is illegal
   */
  private final Feature m_feature;

  /**
   * Qname of the property that is Illegal
   */
  private final QName m_propQName;

  /**
   * The property value found illegal
   */
  private final Object m_propValue;

  public IllegalFeatureState( Feature feature, QName propQName, Object propValue )
  {
    this( computeMessage( feature, propQName, propValue ), feature, propQName, propValue );
  }

  /**
   * Constructor an illegal feature state
   * 
   * @param message
   *          the exception message
   * @param feature
   *          the feature with the illegal state
   * @param propQName
   *          the Q-name of the property, which value found illegal
   * @param propValue
   *          the property value found illegal
   */
  public IllegalFeatureState( String message, Feature feature, QName propQName, Object propValue )
  {
    super( message );
    this.m_feature = feature;
    this.m_propQName = propQName;
    this.m_propValue = propValue;
  }

  static private final String computeMessage( Feature feature, QName propQName, Object propValue )
  {
    StringBuffer buf = new StringBuffer( 128 );
    buf.append( "Illegal feature State" );
    if( feature != null )
    {
      buf.append( "\tfeature:" );
      buf.append( feature );
      if( propQName != null )
      {
        buf.append( "n\tIllegal property:" );
        buf.append( propQName );
        buf.append( "\n\t\tpropValue:" );
        buf.append( propValue );

      }
    }
    return buf.toString();
  }

  public QName getPropQName( )
  {
    return m_propQName;
  }

  public Object getPropValue( )
  {
    return m_propValue;
  }

  /**
   * To get the feature with the illegal state
   * 
   * @return the feature feature with the illegal state
   */
  public Feature getFeature( )
  {
    return m_feature;
  }

}
