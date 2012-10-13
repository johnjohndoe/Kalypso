package org.kalypso.model.wspm.tuhh.core.profile.pattern;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.result.ProfileAndResults;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;

import com.google.common.cache.CacheLoader;

/**
 * @author Gernot Belger
 */
public final class ResultFinder extends CacheLoader<Pair<IProfileFeature, String>, WspmResultLengthSection>
{
  @Override
  public WspmResultLengthSection load( final Pair<IProfileFeature, String> input ) throws Exception
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
    final Object element = ProfileAndResults.findResultNode( profileFeature );
    if( !(element instanceof IWspmResultNode) )
      return null;

    final IWspmResultNode node = (IWspmResultNode) element;
    return findResultByName( node, nodeID );
  }

  private static IWspmResult findResultByName( final IWspmResultNode node, final String name )
  {
    if( node instanceof IWspmResult )
    {
      final IWspmResultNode parent = node.getParent();
      final String parentLabel = parent.getLabel();
      if( ObjectUtils.equals( name, parentLabel ) )
        return (IWspmResult) node;
    }

    // REMARK: tricky: this relies on the fact, that the current result ('_aktuell')
    // is always the first element
    final IWspmResultNode[] childNodes = node.getChildResults();
    for( final IWspmResultNode childNode : childNodes )
    {
      final IWspmResult result = findResultByName( childNode, name );
      if( result != null )
        return result;
    }

    return null;
  }
}