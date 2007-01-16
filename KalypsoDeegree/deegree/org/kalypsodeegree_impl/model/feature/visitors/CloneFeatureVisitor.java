package org.kalypsodeegree_impl.model.feature.visitors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * <p>
 * copy a subtree feature structure inside a gml document
 * </p>
 * start visitor with the parent feature of the subtree (the one that you do not want to copy)
 * 
 * @author hübsch, doemming
 */
public class CloneFeatureVisitor implements FeatureVisitor
{
  private final Map m_idMap = new HashMap();

  final GMLWorkspace m_workspace;

  private final Feature m_targetFeature;

  private final IPropertyType[] m_ftp;

  private final Map m_linkInfoHash = new HashMap();

  public CloneFeatureVisitor( final GMLWorkspace workspace, final Feature targetFeature, final IPropertyType[] ftp )
  {
    m_workspace = workspace;
    m_targetFeature = targetFeature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public synchronized boolean visit( final Feature parentFEOriginal )
  {
    Feature parentFEClone = getFeatureClone( parentFEOriginal );
    if( parentFEClone == null )
      parentFEClone = m_targetFeature;

    IPropertyType[] propertiesToCopy = parentFEOriginal.getFeatureType().getProperties();

    if( parentFEClone == m_targetFeature )
      propertiesToCopy = m_ftp;

    for( int i = 0; i < propertiesToCopy.length; i++ )
    {

      final IPropertyType property = propertiesToCopy[i];
      if( property instanceof IRelationType )
      {
        IRelationType linkPT = (IRelationType) property;
        final Feature[] childFEsOriginal = m_workspace.resolveLinks( parentFEOriginal, linkPT, GMLWorkspace.RESOLVE_ALL );
        for( int j = 0; j < childFEsOriginal.length; j++ )
        {
          final Feature childFEOriginal = childFEsOriginal[j];
          // clone child features
          if( m_workspace.isAggregatedLink( parentFEOriginal, linkPT, j ) )
          { // aggregation
            try
            {
              // a. link target is in tree to copy and link target is allready cloned in target tree
              // b. link target is in tree to copy and link target is not jet cloned in target tree
              // c. link target is outside tree to copy

              final Feature childFEClone = getFeatureClone( childFEOriginal );
              if( childFEClone != null ) // a
                m_workspace.addFeatureAsAggregation( parentFEClone, linkPT, j, childFEClone.getId() );
              else
              {// b or c
                m_workspace.addFeatureAsAggregation( parentFEClone, linkPT, j, childFEOriginal.getId() );
                addLinkInfo( new AdaptLinkInfo( parentFEOriginal, linkPT, j, childFEOriginal ) );
              }
            }
            catch( Exception e ) // do nothing, assume that original features is valid
            {
              e.printStackTrace();
            }
          }
          else
          // composition
          {
            final Feature childFEClone = m_workspace.createFeature( parentFEClone, linkPT, childFEOriginal.getFeatureType() );
            try
            {
              m_workspace.addFeatureAsComposition( parentFEClone, linkPT, j, childFEClone );
              addToIdMap( childFEOriginal.getId(), childFEClone.getId() );
              final AdaptLinkInfo[] linkInfo = getLinkInfo( childFEOriginal );
              for( int k = 0; k < linkInfo.length; k++ )
              {
                final AdaptLinkInfo info = linkInfo[k];
                info.adaptToClones();
              }
            }
            catch( Exception e )// do nothing, assume that original features is valid
            {
              e.printStackTrace();
            }
          }
        }
      }
      else
      {
        // simple property
        try
        {
          FeatureHelper.copySimpleProperty( parentFEOriginal, parentFEClone, property );
        }
        catch( CloneNotSupportedException e ) // TODO what to do with the exceptions here ?
        {
          e.printStackTrace();
        }
      }
    }
    return true;
  }

  /**
   * @param targetFEOriginal
   * @return linkInfo or null if no linkinfo present
   */
  private AdaptLinkInfo[] getLinkInfo( final Feature targetFEOriginal )
  {
    final List list = (List) m_linkInfoHash.get( targetFEOriginal );
    if( list == null )
      return new AdaptLinkInfo[0];
    return (AdaptLinkInfo[]) list.toArray( new AdaptLinkInfo[list.size()] );
  }

  /**
   * @param linkInfo
   */
  private void addLinkInfo( final AdaptLinkInfo linkInfo )
  {
    final Object key = linkInfo.getTargetFeature();
    if( !m_linkInfoHash.containsKey( key ) )
      m_linkInfoHash.put( key, new ArrayList() );

    final List list = (List) m_linkInfoHash.get( key );
    list.add( linkInfo );
  }

  /**
   * returns clone of parentFEOriginal in this copy procedure
   * 
   * @param parentFEOriginal
   * @return clone of parentFEOriginal
   */
  Feature getFeatureClone( Feature parentFEOriginal )
  {
    final String parentID = parentFEOriginal.getId();
    if( m_idMap.containsKey( parentID ) )
    {
      final String cloneID = (String) m_idMap.get( parentID );
      return m_workspace.getFeature( cloneID );
    }
    return null;
  }

  /**
   * mapping from original feature to cloned feature in this copy procedure
   * 
   * @param fidOriginal
   * @param fidClone
   */
  private void addToIdMap( final String fidOriginal, final String fidClone )
  {
    m_idMap.put( fidOriginal, fidClone );
  }

  private class AdaptLinkInfo
  {
    private final Feature m_parentFEOriginal;

    private final int m_pos;

    private final Feature m_targetFEOriginal;

    private final IRelationType m_linkPT;

    public AdaptLinkInfo( final Feature parentFEOriginal, IRelationType linkPT, final int pos, final Feature targetFEOriginal )
    {
      m_parentFEOriginal = parentFEOriginal;
      m_linkPT = linkPT;
      m_pos = pos;
      m_targetFEOriginal = targetFEOriginal;
    }

    /**
     * @return target feature
     */
    public Feature getTargetFeature( )
    {
      return m_targetFEOriginal;
    }

    /**
     * @throws Exception
     */
    public void adaptToClones( ) throws Exception
    {
      final Feature srcFEClone = getFeatureClone( m_parentFEOriginal );
      final Feature targetFEClone = getFeatureClone( m_targetFEOriginal );
      m_workspace.setFeatureAsAggregation( srcFEClone, m_linkPT, m_pos, targetFEClone.getId() );
    }
  }
}
