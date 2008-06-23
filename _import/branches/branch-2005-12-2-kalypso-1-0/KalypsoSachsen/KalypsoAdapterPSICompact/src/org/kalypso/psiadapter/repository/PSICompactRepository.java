package org.kalypso.psiadapter.repository;

import java.util.Map;
import java.util.TreeMap;

import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.psiadapter.util.ObjectInfoLengthComparator;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ObjectInfo;
import de.psi.go.lhwz.PSICompact.ObjectMetaData;

/**
 * Spezifisches Repository für die PSICompact Struktur.
 * 
 * @author schlienger
 */
public class PSICompactRepository extends AbstractRepository
{
  public static String IDENTIFIER = "psicompact://";

  private PSICompactItem m_psiRoot = null;

  public PSICompactRepository( String name, boolean readOnly ) throws RepositoryException
  {
    super( name, PSICompactRepositoryFactory.class.getName(), "", readOnly );

    reload();
  }

  /**
   * Helper um die PSICompact ObjectInfos in einer Repository enabled Struktur umzuwandeln.
   */
  private final void buildStructure( final PSICompactItem rootItem, final Map nodes, int valueType )
      throws ECommException
  {
    final PSICompact psiCompact = PSICompactFactory.getConnection();
    final ObjectInfo[] objInfos = psiCompact.getInfo( valueType );

    java.util.Arrays.sort( objInfos, new ObjectInfoLengthComparator() );

    for( int k = 0; k < objInfos.length; k++ )
    {
      final ObjectInfo info = objInfos[k];
      final String infoID = info.getId().trim();

      final ObjectMetaData objectMetaData = psiCompact.getObjectMetaData( infoID );
      final int[] archiveData = objectMetaData.getArchiveData();

      final String[] path = infoID.split( "\\." );

      // the beginning of each path will be a child of the rootItem
      PSICompactItem parent = rootItem;
      for( int i = 0; i < path.length; i++ )
      {
        final String pathSegment = path[i];
        if( pathSegment.length() == 0 )
          continue;

        final String nodeID = Arrays.implode( path, ".", 0, i ).trim();
        if( nodeID.length() == 0 )
          continue;

        if( nodes.containsKey( nodeID ) )
          parent = (PSICompactItem)nodes.get( nodeID );
        else
        {
          final String name = pathSegment;
          final String obsName = constructName( path, i );

          final int defaultArcType;
          if( archiveData == null || archiveData.length == 0 )
            defaultArcType = PSICompact.ARC_MIN15; // for backwards compability, if unknown use MIN15
          else
            defaultArcType = archiveData[0]; // just take the one with the smallest resolution (the archivedata is
          // considered to be sorted)

          final PSICompactItem newNode = addNewItem( name, obsName, valueType, info, objectMetaData, parent, nodeID,
              defaultArcType, false );
          nodes.put( nodeID, newNode );

          if( PSICompactFactory.hasWQTable( nodeID ) )
            addAtNode( nodes, obsName, valueType, info, objectMetaData, newNode, nodeID, defaultArcType );

          /* Add archive sub-nodes: only if at the end of a real path */
          if( i == path.length - 1 && i > 4 )
          {
            if( archiveData != null )
            {
              for( int j = 0; j < archiveData.length; j++ )
              {
                final int arcType = archiveData[j];

                final String arcName = PSICompactUtilitites.getLabelForArcType( arcType );
                final String arcObsName = obsName;
                final String arcNodeID = nodeID + "#" + PSICompactUtilitites.getIdForArcType( arcType );

                final PSICompactItem newArcNode = addNewItem( arcName, arcObsName, valueType, info, objectMetaData,
                    newNode, arcNodeID, arcType, false );
                nodes.put( arcNodeID, newArcNode );

                // The archive types automaticallay gets this option, if the default has it; we do not need to configure
                // an at-file for each archive
                if( PSICompactFactory.hasWQTable( nodeID ) )
                  addAtNode( nodes, arcObsName, valueType, info, objectMetaData, newArcNode, arcNodeID, defaultArcType );
              }
            }
          }

          parent = newNode;
        }
      }
    }
  }

  /**
   * Adds an at-node to a node, if an at-entry in the config exists.
   */
  private void addAtNode( final Map nodes, final String obsName, int valueType, final ObjectInfo info,
      final ObjectMetaData objectMetaData, final PSICompactItem parent, final String nodeID, final int defaultArcType )
  {
    final String atNodeId = nodeID + "#at";
    final PSICompactItem atNode = addNewItem( "at", obsName, valueType, info, objectMetaData, parent, atNodeId,
        defaultArcType, true );
    nodes.put( atNodeId, atNode );
  }

  /**
   * Creates a new item and adds it as a child to the given parent.
   * 
   * @param arcType
   * 
   * @return The newly created item.
   */
  private PSICompactItem addNewItem( final String name, final String obsName, int valueType, final ObjectInfo info,
      final ObjectMetaData objectMetaData, PSICompactItem parent, final String nodeID, final int arcType,
      final boolean useAt )
  {
    final PSICompactItem newItem = new PSICompactItem( this, parent, name, nodeID, info, valueType, obsName,
        objectMetaData, arcType, useAt );

    // gleich parent item aktualisieren (wird nicht von der Child gemacht,
    // deswegen hier)
    if( parent != null )
      parent.addChild( newItem );

    return newItem;
  }

  private String constructName( final String[] path, final int i )
  {
    // HACK: use path-segment of parent instead of own, if we are a leaf...
    if( i == 6 )
      return path[5];

    return path[i];
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_psiRoot.hasChildren();
  }

  /**
   * 
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return m_psiRoot.getChildren();
  }

  /**
   * Always returns
   * 
   * <pre>
   *  psicompact://
   * </pre>
   * 
   * @see org.kalypso.repository.IRepository#getIdentifier()
   */
  public String getIdentifier()
  {
    return IDENTIFIER;
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public final void reload() throws RepositoryException
  {
    try
    {
      final PSICompactItem rootItem = new PSICompactItem( this, null, "", "", null, -1, "", null, PSICompact.ARC_UNDEF,
          false );
      final TreeMap nodes = new TreeMap();

      buildStructure( rootItem, nodes, PSICompact.TYPE_MEASUREMENT );
      buildStructure( rootItem, nodes, PSICompact.TYPE_VALUE );

      m_psiRoot = rootItem;

      fireRepositoryStructureChanged();
    }
    catch( final Exception e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( String id ) throws RepositoryException
  {
    return findItemRecursive( m_psiRoot, id );
  }
}