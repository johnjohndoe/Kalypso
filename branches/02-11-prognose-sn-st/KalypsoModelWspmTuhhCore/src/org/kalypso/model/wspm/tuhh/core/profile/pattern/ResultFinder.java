package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import org.apache.commons.lang.ObjectUtils;
import org.kalypso.commons.pair.IKeyValue;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.result.ProfileAndResults;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;

import com.google.common.base.Function;

/**
 * @author Gernot Belger
 */
public final class ResultFinder implements Function<IKeyValue<IProfileFeature, String>, WspmResultLengthSection>
{
  /**
   * @see com.google.common.base.Function#apply(java.lang.Object)
   */
  @Override
  public WspmResultLengthSection apply( final IKeyValue<IProfileFeature, String> input )
  {
    final IProfileFeature profileFeature = input.getKey();
    final String nodeID = input.getValue();

    final IWspmResult result = findResult( profileFeature, nodeID );
    if( result == null )
      return null;

    return result.getLengthSection();
  }

  public static IWspmResult findResult( final IProfileFeature profileFeature, final String nodeID )
  {
    final Object node = ProfileAndResults.findResultNode( profileFeature );
    if( !(node instanceof IWspmResultNode) )
      return null;

    final IWspmResultNode resultNode = findNodeByName( ((IWspmResultNode) node), nodeID );
    return findResult( resultNode );
  }

  private static IWspmResult findResult( final IWspmResultNode node )
  {
    if( node instanceof IWspmResult )
      return (IWspmResult) node;

    final IWspmResultNode[] childNodes = node.getChildResults();
    for( final IWspmResultNode childNode : childNodes )
    {
      final IWspmResult result = findResult( childNode );
      if( result != null )
        return result;
    }

    return null;
  }

  private static IWspmResultNode findNodeByName( final IWspmResultNode node, final String name )
  {
    final String nodeName = node.getLabel();
    if( ObjectUtils.equals( name, nodeName ) )
      return node;

    final IWspmResultNode[] childNodes = node.getChildResults();
    for( final IWspmResultNode childNode : childNodes )
    {
      final IWspmResultNode result = findNodeByName( childNode, name );
      if( result != null )
        return result;
    }

    return null;
  }

}