package org.kalypsodeegree_impl.gml.binding.math;

import java.math.BigInteger;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Patrice Congo
 */
public class Polynomial2D extends AbstractFeatureBinder implements IPolynomial2D
{
  public static final QName QNAME = new QName( NS.COMMON_MATH, "Polynomial2D" );

  public static final QName QNAME_PROP_DEGREEX = new QName( NS.COMMON_MATH, "degreeX" );

  public static final QName QNAME_PROP_DEGREEY = new QName( NS.COMMON_MATH, "degreeY" );

  public static final QName QNAME_PROP_COEFFICIENTS = new QName( NS.COMMON_MATH, "coefficients" );

  public Polynomial2D( final Feature polFeature )
  {
    super( polFeature, QNAME );
  }

  public PolynomialConfigState checkConsistency( )
  {
    return null;
  }

  public static final PolynomialConfigState checkConsistency( int degreeX, int degreeY, double[] coefficients )
  {
    if( coefficients == null )
    {
      throw new IllegalArgumentException( "argument coefs must not be null" );
    }

    if( degreeX < 0 )
    {
      return PolynomialConfigState.NEGATIVE_DEGREEX;
    }

    if( degreeY < 0 )
    {
      return PolynomialConfigState.NEGATIVE_DEGREEY;
    }

    if( (degreeY + 1) * (degreeX + 1) != coefficients.length )
    {
      return PolynomialConfigState.ORDER_COEF_MISMATCH;
    }
    // TODO check last x and last y
    if( coefficients[coefficients.length - 1] == 0.0 )
    {
      return PolynomialConfigState.ZERO_MOST_SIGNIFICANT_COEFS;
    }
    return PolynomialConfigState.CONSISTENCY_OK;
  }

  public double evaluate( double inputX, double inputY )
  {
    throw new RuntimeException( "not supported" );
  }

  public double[] getCoefficients( ) throws IllegalFeatureState
  {
    Object coefs = getFeature().getProperty( QNAME_PROP_COEFFICIENTS );
    if( coefs instanceof String )
    {
      String[] subStrings = ((String) coefs).split( " " );// "/s+");
      double doubles[] = new double[subStrings.length];
      for( int i = 0; i < subStrings.length; i++ )
      {
        doubles[i] = Double.parseDouble( subStrings[i] );
      }
      return doubles;
    }
    else
    {
      throw new IllegalFeatureState( getFeature(), QNAME_PROP_COEFFICIENTS, coefs );
    }
  }

  public void setCefficients( double[] coefficients ) throws IllegalArgumentException
  {

  }

  public int getDegreeX( ) throws IllegalFeatureState
  {
    return getDegree( QNAME_PROP_DEGREEX );
  }

  public void setDegreeX( int degreeX )
  {
    setDegree( degreeX, QNAME_PROP_DEGREEX );
  }

  public int getDegreeY( ) throws IllegalFeatureState
  {
    return getDegree( QNAME_PROP_DEGREEY );
  }

  public void setDegreeY( int degreeY )
  {
    setDegree( degreeY, QNAME_PROP_DEGREEY );
  }

  private final void setDegree( int degree, QName degreeQName ) throws IllegalArgumentException
  {

    if( degree <= 0 )
    {
      throw new IllegalArgumentException();
    }

    getFeature().setProperty( degreeQName, new Integer( degree ) );
  }

  private final int getDegree( QName degreeQName ) throws IllegalFeatureState
  {
    Object dx = getFeature().getProperty( degreeQName );
    if( dx instanceof BigInteger )
    {
      return ((BigInteger) dx).intValue();
    }
    else
    {
      StringBuffer buf = new StringBuffer( 128 );
      buf.append( degreeQName );
      buf.append( " must be and integer but got:" );
      buf.append( dx == null ? null : dx.getClass() );
      buf.append( "with the value:" );
      buf.append( dx );
      throw new IllegalFeatureState( buf.toString(), getFeature(), degreeQName, dx );

    }

  }

  public void setPolynomParameters( int degreeX, int degreeY, double[] coefficients ) throws IllegalArgumentException
  {
    if( PolynomialConfigState.CONSISTENCY_OK != checkConsistency( degreeX, degreeY, coefficients ) )
    {
      throw new IllegalArgumentException();
    }

    StringBuffer buf = new StringBuffer( 128 );
    for( double coef : coefficients )
    {
      buf.append( coef );
      buf.append( ' ' );
    }
    getFeature().setProperty( QNAME_PROP_COEFFICIENTS, buf.toString() );
  }

}
