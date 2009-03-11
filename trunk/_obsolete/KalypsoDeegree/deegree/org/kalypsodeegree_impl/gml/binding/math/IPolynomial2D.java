/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.gml.binding.math;


/**
 * Specifies the interface for object representing a 2D polynom in the simulation model base schema. The algerbic
 * representation is: SUM(aijx^iy^j) for 0&lt=i&lt=degreeX and 0&lt=i&lt=degreeY
 * 
 * @author Patrice Congo
 */
public interface IPolynomial2D extends IPolynomial
{
  /**
   * To get the x degree of this polynom.
   * 
   * @return the x degree corresponding the given index.
   * @throws IllegalFeatureState
   */
  public int getDegreeX( ) throws IllegalFeatureState;

  public void setDegreeX( int degreeX );

  /**
   * To get the y degree of the polynom
   * 
   * @return the y degree of the polynom
   */
  public int getDegreeY( ) throws IllegalFeatureState;

  public void setDegreeY( int degreeY );

  /**
   * Return all the coeficients of the array as 1 dimentional array. Note that for polynom with oder equals and greater
   * 2 there is a mapping from a to and more dimentional array to a one dimentional array. For 2 dimentional coefficient
   * aij will be found a index i*(getDegreeX)+j in the return array.
   * 
   * @return a doouble array containing all the polynom coefficients
   * @throws IllegalFeatureState
   */
  public double[] getCoefficients( ) throws IllegalFeatureState;

  public void setCefficients( double[] coefficients ) throws IllegalArgumentException;

  /**
   * Sets the polynom parameter, which are its order, its dimention and the coefficients
   * 
   * @param degreeX
   *          the new x degree of the polynom
   * @param degreeY
   *          the new y degree of the polynom
   * @param coefficients --
   *          the coefficient of this polynom
   * @throws illegal
   *           argument exception if
   *           <ul>
   *           <li/>degreeX or degreeY is negative <li/>coefficient is null <li/>number of double passed as coefficient
   *           is not equal to order (deegreeX+1)*(degreeY+1) and their respective last element are not zero
   *           </ul>
   */
  public void setPolynomParameters( int degreeX, int degreeY, double[] coefficients ) throws IllegalArgumentException;

  /**
   * Evaluates to polynomial for the given value.
   * 
   * @param inputX
   *          a double for which the x input is to be computed
   * @param inputY
   *          a double for which the y input is to be computed
   * @return the polynom value of the given result.
   */
  public double evaluate( double inputX, double inputY );

  // public PolynomialConfigState checkConsistency();
}