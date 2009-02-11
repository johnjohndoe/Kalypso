/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.featureview.control;


/**
 * @author Gernot Belger
 */
public class ExpressionFactory
{
// /**
// * Given a DOM-fragment, a corresponding Expression-object is built. This method recursively calls other buildFromDOM
// () -
// * methods to validate the structure of the DOM-fragment.
// *
// * @throws FilterConstructionException
// * if the structure of the DOM-fragment is invalid
// */
// public static Expression buildExpression( final JAXBElement< ? extends ExpressionType> expression ) throws
// FilterConstructionException
// {
// final String localPart = expression.getName().getLocalPart();
//
// final int id = Expression_Impl.EXPRESSION_DEFINES.getIdByName( localPart );
//
// switch( id )
// {
// case ExpressionDefines.EXPRESSION:
// throw new FilterConstructionException( "Expression is abstract." );
//
// case ExpressionDefines.PROPERTYNAME:
// {
// final PropertyNameType propertyNameType = (PropertyNameType) expression.getValue();
// // TODO
// final String xpath = "TODO";
// return new PropertyName( xpath, null );
// }
// case ExpressionDefines.LITERAL:
// {
// final LiteralType literalType = (LiteralType) expression.getValue();
// // TODO
// final String literal = "" + literalType.getAny();
// return new Literal( literal );
// }
// case ExpressionDefines.FUNCTION:
// {
// final FunctionType functionType = (FunctionType) expression.getValue();
// final String name = functionType.getName();
// final List<Expression> functionExpressions = buildSubExpressions( functionType.getExpression() );
//
// return new Function( name, functionExpressions );
// }
// case ExpressionDefines.ADD:
// case ExpressionDefines.SUB:
// case ExpressionDefines.MUL:
// case ExpressionDefines.DIV:
// {
// final BinaryOperatorType binaryOperatorType = (BinaryOperatorType) expression.getValue();
// final List<Expression> functionExpressions = buildSubExpressions( binaryOperatorType.getExpression() );
//
// // determine the arguments
// if( functionExpressions.size() != 2 )
// throw new FilterConstructionException( "'" + localPart + "' requires exactly 2 elements!" );
//
// final Expression operand1 = functionExpressions.get( 0 );
// final Expression operand2 = functionExpressions.get( 1 );
// return new ArithmeticExpression( id, operand1, operand2 );
// }
// default:
// {
// throw new FilterConstructionException( "Unknown expression '" + localPart + "'!" );
// }
// }
// }
//
// private static List<Expression> buildSubExpressions( final List<JAXBElement< ? extends ExpressionType>>
// expressionElements ) throws FilterConstructionException
// {
// final List<Expression> functionExpressions = new ArrayList<Expression>( expressionElements.size() );
// for( final JAXBElement< ? extends ExpressionType> functionExpression : expressionElements )
// functionExpressions.add( buildExpression( functionExpression ) );
// return functionExpressions;
// }
//
// public AbstractOperation buildOperation( final JAXBElement< ? > element ) throws FilterConstructionException
// {
// // check if root element's name is a known operator
// final String name = element.getName().getLocalPart();
// final int type = OperationDefines.getTypeByName( name );
// final int id = OperationDefines.getIdByName( name );
//
// final Object value = element.getValue();
//
// if( value instanceof BinaryComparisonOpType )
// {
// final BinaryComparisonOpType binComOp = (BinaryComparisonOpType) value;
// final List<Expression> subExpressions = buildSubExpressions( binComOp.getExpression() );
//
// return new PropertyIsCOMPOperation( id, subExpressions.get( 0 ), subExpressions.get( 1 ) );
// }
// else if( value instanceof PropertyIsLikeType )
// {
// final PropertyIsLikeType isLike = (PropertyIsLikeType) value;
// // check if root element's name equals 'PropertyIsLike'
//
// final PropertyNameType propertyName = isLike.getPropertyName();
// final LiteralType literal = isLike.getLiteral();
// final ElementList children = XMLTools.getChildElements( element );
// if( children.getLength() != 2 )
// throw new FilterConstructionException( "'PropertyIsLike' requires exactly 2 elements!" );
//
// final PropertyName propertyName = (PropertyName) PropertyName.buildFromDOM( children.item( 0 ) );
// final Literal literal = (Literal) Literal.buildFromDOM( children.item( 1 ) );
//
// // determine the needed attributes
// final String wildCard = element.getAttribute( "wildCard" );
// if( wildCard == null || wildCard.length() == 0 )
// throw new FilterConstructionException( "wildCard-Attribute is unspecified!" );
// if( wildCard.length() != 1 )
// throw new FilterConstructionException( "wildCard-Attribute must be exactly one character!" );
// final String singleChar = element.getAttribute( "singleChar" );
// if( singleChar == null || singleChar.length() == 0 )
// throw new FilterConstructionException( "singleChar-Attribute is unspecified!" );
// if( singleChar.length() != 1 )
// throw new FilterConstructionException( "singleChar-Attribute must be exactly one character!" );
// String escapeChar = element.getAttribute( "escape" );
// if( escapeChar == null || escapeChar.length() == 0 )
// escapeChar = element.getAttribute( "escapeChar" );
// if( escapeChar == null || escapeChar.length() == 0 )
// throw new FilterConstructionException( "escape-Attribute is unspecified!" );
// if( escapeChar.length() != 1 )
// throw new FilterConstructionException( "escape-Attribute must be exactly one character!" );
//
// return new PropertyIsLikeOperation( propertyName, literal, wildCard.charAt( 0 ), singleChar.charAt( 0 ),
// escapeChar.charAt( 0 ) );
//
// }
//
// switch( id )
// {
// /* Spatial Operations */
// // not supported
// case OperationDefines.PROPERTYISLIKE:
// {
// return (ComparisonOperation) PropertyIsLikeOperation.buildFromDOM( element );
// break;
// }
// case OperationDefines.PROPERTYISNULL:
// {
// operation = (ComparisonOperation) PropertyIsNullOperation.buildFromDOM( element );
// break;
// }
// case OperationDefines.PROPERTYISBETWEEN:
// {
// operation = (ComparisonOperation) PropertyIsBetweenOperation.buildFromDOM( element );
// break;
// }
//
// case OperationDefines.TYPE_LOGICAL:
// return buildLogicalOperation( (LogicOpsType) value );
// default:
// throw new FilterConstructionException( "Unknown operator '" + name + "'!" );
// }
// }
//
// private SpatialOperation buildSpatialOperation( final SpatialOpsType type )
// {
// throw new UnsupportedOperationException();
// }
//
// private LogicalOperation buildLogicalOperation( final LogicOpsType type )
// {
// Expression_Impl.buildFromDOM( element );
//
// // TODO Auto-generated method stub
// return null;
// }
//
}
