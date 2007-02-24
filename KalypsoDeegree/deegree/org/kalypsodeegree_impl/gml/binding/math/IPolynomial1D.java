package org.kalypsodeegree_impl.gml.binding.math;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Specifies the interface for object representing a 1D polynom in the simulation model base schema
 * 
 * <pre>
 * &amp;ltxs:complexType 
 * 	name=&quot;Polynomial1DType&quot;
 * 	abstract=&quot;false&quot;&gt;
 * 	&amp;ltxs:annotation&gt;
 * 		&amp;ltxs:documentation&amp;gt
 * 			Type for 1D polynoms: y=sum of ai*xi for all i less than oder
 * 		&amp;lt/xs:documentation&amp;gt
 * 	&amp;lt/xs:annotation&amp;gt
 * 	&amp;ltxs:complexContent&amp;gt
 * 		&amp;ltxs:extension 
 * 			base=&quot;simBase:AbstractPolynomialType&quot;&amp;gt
 * 			&amp;ltxs:sequence&amp;gt
 * 				&amp;ltxs:element 
 * 					name=&quot;order&quot;
 * 					type=&quot;xs:positiveInteger&quot;
 * 					minOccurs=&quot;1&quot; maxOccurs=&quot;1&quot;/&amp;gt
 * 				&amp;ltxs:element
 * 					name=&quot;coefficients&quot;
 * 					type=&quot;gml:doubleOrNullList&quot;/&amp;gt
 * 			&amp;lt/xs:sequence&amp;gt
 * 		&amp;lt/xs:extension&amp;gt
 * 	&amp;lt/xs:complexContent&amp;gt
 * &amp;lt/xs:complexType&amp;gt
 * </pre>
 * 
 * @author Patrice Congo
 */
public interface IPolynomial1D extends IPolynomial, IFeatureWrapper2
{
  public static final QName QNAME = new QName( NS.COMMON_MATH, "Polynomial1D" );

  public static final QName QNAME_PROP_COEFFICIENTS = new QName( NS.COMMON_MATH, "coefficients" );

  public static final QName QNAME_PROP_MINRANGE = new QName( NS.COMMON_MATH, "minRange" );

  public static final QName QNAME_PROP_MAXRANGE = new QName( NS.COMMON_MATH, "maxRange" );

  public static final QName QNAME_PROP_DOMAIN_PHENOMENON = new QName( NS.COMMON_MATH, "domainPhenomenon" );

  public static final QName QNAME_PROP_RANGE_PHENOMENON = new QName( NS.COMMON_MATH, "rangePhenomenon" );

  /**
   * Return all the coeficients of the array as 1 dimentional array.
   * 
   * @return a double array containing all the polynom coefficients
   */
  public double[] getCoefficients( );

  /**
   * Set coefficients without a realtional check with the already set coefficient.
   * 
   * @param coefficients
   *          the coefficients to set
   * @throws IllegalArgumentException
   *           if coefficients is null or the last element in coefficient is zero (since this has an implication when
   *           checking the order of the polynom).
   */
  public void setCoefficients( final double[] coefficients ) throws IllegalArgumentException;

  /**
   * Sets the range of validity for this polynome.
   * 
   * @param from
   *          minimal domain value this polynome is valid for . If Double.NaN or Double.NEGATIVE_INFINITY is given no
   *          minimal range is used.
   * @param to
   *          maximal domain value this polynome is valid for. If Double.NaN or Double.POSITIVE_INFINITYis given no
   *          maximal range is used.
   */
  public void setRange( final double from, final double to );

  /**
   * Returns the minimal domain value this polynome is valid for.
   * 
   * @return May return Double.NEGATIVE_INFINITY
   * @see #setRange(double, double)
   */
  public double getRangeMin( );

  /**
   * Returns the positive domain value this polynome is valid for.
   * 
   * @return May return Double.POSITIVE_INFINITY
   * @see #setRange(double, double)
   */
  public double getRangeMax( );

  /**
   * compute the polynom value for the given value.
   * 
   * @param input
   *          a double for which the input is to be computed
   * @return the polynom value of the given result.
   */
  public double computeResult( double input );

  public void setDomainPhenomenon( final String domainId );

  public void setRangePhenomenon( final String rangeId );

  public String getDomainPhenomenon( );

  public String getRangePhenomenon( );
}
