/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  DenickestraÃŸe 22
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
package org.kalypso.risk.eval;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.kalypso.risk.eval.function.IEvalFunctionMember;
import org.kalypso.risk.eval.function.RPNEntry;
import org.kalypso.risk.eval.function.RPNEntry.EEntryType;
import org.kalypso.risk.eval.function.member.EvalFunctionMember_Add;
import org.kalypso.risk.eval.function.member.EvalFunctionMember_Div;
import org.kalypso.risk.eval.function.member.EvalFunctionMember_Mul;
import org.kalypso.risk.eval.function.member.EvalFunctionMember_Pow;
import org.kalypso.risk.eval.function.member.EvalFunctionMember_Sub;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class ExpressionParser
{
  private final List<IEvalFunctionMember> m_evalFunctionMembers = new ArrayList<IEvalFunctionMember>();

  private final List<RPNEntry> m_postfixExpression = new ArrayList<RPNEntry>();

  public ExpressionParser( final String expression )
  {
    m_evalFunctionMembers.add( new EvalFunctionMember_Pow() );
    m_evalFunctionMembers.add( new EvalFunctionMember_Add() );
    m_evalFunctionMembers.add( new EvalFunctionMember_Sub() );
    m_evalFunctionMembers.add( new EvalFunctionMember_Mul() );
    m_evalFunctionMembers.add( new EvalFunctionMember_Div() );
    convertToPostfix( expression.replaceAll( "\\s", "" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private final void convertToPostfix( final String expression )
  {
    final Stack<RPNEntry> stack = new Stack<RPNEntry>();

    String currentExpression = expression;

    final Pattern patternNumber = Pattern.compile( "(\\d*[\\.,]{0,1}\\d+).*" ); //$NON-NLS-1$

    while( currentExpression.length() > 0 )
    {
      final Matcher numberMatcher = patternNumber.matcher( currentExpression );
      if( numberMatcher.matches() )
      {
        final String numberStr = numberMatcher.group(1);
        currentExpression = currentExpression.substring( numberStr.length() );
        final Double number = Double.valueOf( numberStr.replaceFirst( ",", "." ) ); //$NON-NLS-1$ //$NON-NLS-2$
        m_postfixExpression.add( new RPNEntry( number ) );
      }
      else if( currentExpression.substring( 0, 1 ).equalsIgnoreCase( "x" ) ) //$NON-NLS-1$
      {
        m_postfixExpression.add( new RPNEntry( EEntryType.PARAMETER ) );
        currentExpression = currentExpression.substring( 1 );
      }
      else if( currentExpression.startsWith( "(" ) ) //$NON-NLS-1$
      {
        stack.push( new RPNEntry( EEntryType.OPEN_BRACKET ) );
        currentExpression = currentExpression.substring( 1 );
      }
      else if( currentExpression.startsWith( ")" ) ) //$NON-NLS-1$
      {
        while( !stack.isEmpty() )
        {
          final RPNEntry opTop = stack.pop();
          if( RPNEntry.EEntryType.OPEN_BRACKET.equals( opTop.getEntryType() ) )
            break;
          else
            m_postfixExpression.add( opTop );
        }
        currentExpression = currentExpression.substring( 1 );
      }
      else
      // if operator ie function
      {
        IEvalFunctionMember function = null;// find out the function
        for( final IEvalFunctionMember functionMember : m_evalFunctionMembers )
        {
          if( currentExpression.startsWith( functionMember.getOperator() ) )
          {
            function = functionMember;
            currentExpression = currentExpression.substring( functionMember.getOperator().length() );
            break;
          }
        }
        if( function == null )
          throw new IllegalArgumentException();

        final RPNEntry entry = new RPNEntry( function );
        while( !stack.isEmpty() )
        {
          final RPNEntry opTop = stack.pop();
          if( RPNEntry.EEntryType.OPEN_BRACKET.equals( opTop.getEntryType() ) )
          {
            stack.push( opTop );
            break;
          }
          else
          {// it's an operator
            if( opTop.getEvalFunctionMember().compareTo( function ) > 0 )
            {
              stack.push( opTop );
              break;
            }
            else
              m_postfixExpression.add( opTop );
          }
        }
        stack.push( entry );
      }
    }
    while( !stack.isEmpty() ){
      m_postfixExpression.add( stack.pop() );
    }
  }

  public final double evaluate( final double value )
  {
    final Stack<Double> stack = new Stack<Double>();
    for( final RPNEntry entry : m_postfixExpression )
    {
      switch( entry.getEntryType() )
      {
        case CONSTANT:
          stack.push( entry.getValue() );
          break;
        case PARAMETER:
          stack.push( value );
          break;
        case OPERATOR:
          final double opResult = entry.getEvalFunctionMember().calculate( stack );
          stack.push( opResult );
          break;
        default:
          throw new IllegalStateException();
      }
    }
    return stack.pop();
  }
  
  public final String getPostfixExpression(){
    final StringBuffer buffer = new StringBuffer();
    for( final RPNEntry entry : m_postfixExpression )
    {
      switch( entry.getEntryType() )
      {
        case CONSTANT:
          buffer.append( entry.getValue() );
          break;
        case PARAMETER:
          buffer.append( "[P]" ); //$NON-NLS-1$
          break;
        case OPERATOR:
          buffer.append( entry.getEvalFunctionMember().getOperator() );
          break;
        default:
          buffer.append( "[ERROR: " ).append( entry.getEntryType().name() ).append( "]" ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      buffer.append( " " ); //$NON-NLS-1$
    }
    return buffer.toString();
  }

}