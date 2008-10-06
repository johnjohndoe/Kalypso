package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author F.Lindemann
 *  
 */
public class BoundaryExpression extends Expression_Impl implements Expression
{

  private String m_value = null;

  public BoundaryExpression( String value1 )
  {
    this.m_value = value1;
  }

  @Override
  public StringBuffer toXML()
  {
    return new StringBuffer( m_value.toString() );
  }

  public Object evaluate( Feature feature ) throws FilterEvaluationException
  {
    Double returnValue = null;
    try
    {
      returnValue = new Double( m_value );
    }
    catch( NumberFormatException e )
    {
      throw new FilterEvaluationException( "BoundaryExpression:  can only be applied to numerical " + "expressions!" );
    }
    return returnValue;
  }

  public String getValue()
  {
    return m_value;
  }

  public void setValue( String value )
  {
    m_value = value;
  }
}