package org.kalypsodeegree_impl.filterencoding;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author F.Lindemann
 *  
 */
public class BoundaryExpression extends Expression_Impl
{

  private String value = null;

  public BoundaryExpression( String m_value )
  {
    this.value = m_value;
  }

  public StringBuffer toXML()
  {
    return new StringBuffer( value.toString() );
  }

  public Object evaluate( Feature feature ) throws FilterEvaluationException
  {
    Double returnValue = null;
    try
    {
      returnValue = new Double( value );
    }
    catch( NumberFormatException e )
    {
      throw new FilterEvaluationException( "BoundaryExpression:  can only be applied to numerical " + "expressions!" );
    }
    return returnValue;
  }

  public String getValue()
  {
    return value;
  }

  public void setValue( String m_value )
  {
    this.value = m_value;
  }
}