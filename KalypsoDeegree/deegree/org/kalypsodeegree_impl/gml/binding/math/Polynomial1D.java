/**
 * 
 */
package org.kalypsodeegree_impl.gml.binding.math;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Default imlementation of the IPolynom1D interface
 * 
 * @author Patrice Congo
 */
public class Polynomial1D extends AbstractFeatureBinder implements IPolynomial1D
{
  public Polynomial1D( final Feature polFeature )
  {
    super( polFeature, QNAME );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#computeResult(double)
   */
  public double computeResult( final double input )
  {
    // computation based on Hornerschema
    final double coefs[] = getCoefficients();

    int i = coefs.length;
    // the empty sum is always 0
    if( i == 0 )
      return 0.0;

    i--;// last element
    double result = coefs[i];
    i--;
    for( ; i >= 0; i-- )
      result = result * input + coefs[i];

    return result;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.util.math.IPolynom1D#getCoefficients()
   */
  @SuppressWarnings("unchecked")
  public double[] getCoefficients( )
  {
    final List<Double> coefs = (List<Double>) getFeature().getProperty( QNAME_PROP_COEFFICIENTS );
    if( coefs == null )
      return new double[0];

    final Double[] objects = coefs.toArray( new Double[coefs.size()] );
    return ArrayUtils.toPrimitive( objects, Double.NaN );
  }

  public void setCoefficients( final double[] coefficients ) throws IllegalArgumentException
  {
    final List<Double> list = new ArrayList<Double>();

    if( coefficients != null )
    {
      for( int i = 0; i < coefficients.length; i++ )
        list.add( coefficients[i] );
    }

    getFeature().setProperty( QNAME_PROP_COEFFICIENTS, list );
  }

  @Override
  public boolean equals( Object obj )
  {
    if( this == obj )
    {
      return true;
    }
    else if( obj instanceof IPolynomial1D )
    {
      double thisCoefs[] = getCoefficients();
      double compCoefs[] = ((IPolynomial1D) obj).getCoefficients();
      int i = thisCoefs.length;
      if( i != compCoefs.length )
      {
        return false;
      }
      i--;// goto last element
      for( ; i >= 0; i-- )
      {
        if( thisCoefs[i] != compCoefs[i] )
        {
          return false;
        }
      }
      return true;

    }
    else
    {
      return false;// super.equals(obj);
    }
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 64 );
    buf.append( "Polynomial1D" );
    final String id = getFeature().getId();
    if( id != null )
    {
      buf.append( '.' );
      buf.append( id );
    }
    // coefs
    buf.append( "[ coefs=" );
    for( final double coef : getCoefficients() )
    {
      buf.append( String.valueOf( coef ) );
      buf.append( ' ' );
    }
    buf.append( ']' );
    return buf.toString();
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial#checkConsistency()
   */
  public PolynomialConfigState checkConsistency( )
  {
    // how should it be ever inconsistent
    return PolynomialConfigState.CONSISTENCY_OK;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#getRangeMin()
   */
  public double getRangeMin( )
  {
    final Double property = (Double) getFeature().getProperty( QNAME_PROP_MINRANGE );
    if( property == null )
      return Double.NEGATIVE_INFINITY;

    return property;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#getRangeMax()
   */
  public double getRangeMax( )
  {
    final Double property = (Double) getFeature().getProperty( QNAME_PROP_MAXRANGE );
    if( property == null )
      return Double.POSITIVE_INFINITY;

    return property;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#setRange(double, double)
   */
  public void setRange( final double from, final double to )
  {
    getFeature().setProperty( QNAME_PROP_MINRANGE, Double.isNaN( from ) ? null : from );
    getFeature().setProperty( QNAME_PROP_MAXRANGE, Double.isNaN( to ) ? null : to );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#setDomainPhenomenon(java.lang.String)
   */
  public void setDomainPhenomenon( final String domainId )
  {
    final Feature ref = refForId( domainId );

    getFeature().setProperty( QNAME_PROP_DOMAIN_PHENOMENON, ref );
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#setRangePhenomenon(java.lang.String)
   */
  public void setRangePhenomenon( final String rangeId )
  {
    final Feature ref = refForId( rangeId );
    
    getFeature().setProperty( QNAME_PROP_RANGE_PHENOMENON, ref );
  }

  private Feature refForId( final String domainId )
  {
    final IRelationType relation = (IRelationType) getFeature().getFeatureType().getProperty( QNAME_PROP_DOMAIN_PHENOMENON );
    final IFeatureType featureType = getFeature().getWorkspace().getGMLSchema().getFeatureType( new QName( NS.SWE, "Phenomenon" ) );

    final Feature ref = new XLinkedFeature_Impl( getFeature(), relation, featureType, domainId, null, null, null, null, null );
    return ref;
  }

  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#getDomainPhenomenon()
   */
  public String getDomainPhenomenon( )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( QNAME_PROP_DOMAIN_PHENOMENON );
    return propertyToId( property );
  }


  /**
   * @see org.kalypsodeegree_impl.gml.binding.math.IPolynomial1D#getRangePhenomenon()
   */
  public String getRangePhenomenon( )
  {
    final Feature feature = getFeature();
    final Object property = feature.getProperty( QNAME_PROP_RANGE_PHENOMENON );
    return propertyToId( property );
  }
  
  private String propertyToId( final Object property )
  {
    if( property == null )
      return null;
    
    if( property instanceof XLinkedFeature_Impl )
      return ((XLinkedFeature_Impl)property).getHref();
    
    if( property instanceof Feature )
      return ((Feature)property).getId();
    
    return property.toString();
  }

}
