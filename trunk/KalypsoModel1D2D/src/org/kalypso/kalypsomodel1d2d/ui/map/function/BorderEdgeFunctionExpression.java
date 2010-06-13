package org.kalypso.kalypsomodel1d2d.ui.map.function;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypsodeegree.filterencoding.Expression;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.filterencoding.AbstractFunctionExpression;

/**
 * Evaluates to <code>true</code> or <code>false</code>, if a 1d2d-Edge is a border-edge or not.
 * 
 * @author Gernot Belger
 */
public class BorderEdgeFunctionExpression extends AbstractFunctionExpression
{
  @Override
  public Object evaluate( final Feature feature, final List<Expression> args ) throws FilterEvaluationException
  {
    final IFE1D2DEdge edge = (IFE1D2DEdge) feature.getAdapter( IFE1D2DEdge.class );
    return edge.isBorder();
  }

}
