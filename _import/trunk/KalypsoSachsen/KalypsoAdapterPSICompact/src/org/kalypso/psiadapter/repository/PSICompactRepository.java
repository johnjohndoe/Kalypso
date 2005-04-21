package org.kalypso.psiadapter.repository;

import java.util.Map;
import java.util.TreeMap;

import org.kalypso.java.util.Arrays;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.psiadapter.util.ObjectInfoLengthComparator;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.ObjectInfo;

/**
 * Spezifisches Repository für die PSICompact Struktur.
 * 
 * @author schlienger
 */
public class PSICompactRepository extends AbstractRepository
{
  private PSICompactItem m_psiRoot = null;

  public PSICompactRepository( String name, boolean readOnly )
      throws RepositoryException
  {
    super( name, PSICompactRepositoryFactory.class.getName(), "", readOnly );

    reload();
  }

  /**
   * Helper um die PSICompact ObjectInfos in einer Repository enabled Struktur
   * umzuwandeln.
   * 
   * @param nodes
   * @param valueType
   * @return item
   * @throws ECommException
   */
  private final PSICompactItem buildStructure( Map nodes, int valueType )
      throws ECommException
  {
    final PSICompact psi = PSICompactFactory.getConnection();

    final ObjectInfo[] objInfos = psi.getInfo( valueType );

    java.util.Arrays.sort( objInfos, new ObjectInfoLengthComparator() );

    PSICompactItem parent = null;

    for( int k = 0; k < objInfos.length; k++ )
    {
      final ObjectInfo info = objInfos[k];
      final String infoID = info.getId().trim();

      final String[] path = infoID.split( "\\." );

      for( int i = 0; i < path.length; i++ )
      {
        if( path[i].length() == 0 )
          continue;

        final String nodeID = Arrays.implode( path, ".", 0, i ).trim();

        if( nodeID.length() == 0 )
          continue;

        if( nodes.containsKey( nodeID ) )
          parent = (PSICompactItem) nodes.get( nodeID );
        else
        {
          final PSICompactItem n = new PSICompactItem( parent, path[i], nodeID,
              info, valueType );

          // gleich parent item aktualisieren (wird nicht von der Child gemacht,
          // deswegen hier)
          if( parent != null )
            parent.addChild( n );

          nodes.put( nodeID, n );

          parent = n;
        }
      }
    }

    // abnormal case...
    if( parent == null )
      return new PSICompactItem( null, "Keine Struktur in PSICompact...",
          "<Kein ID>", new PSICompact.ObjectInfo(), 0 );

    while( parent.getParent() != null )
      parent = (PSICompactItem) parent.getParent();

    return parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren( )
  {
    return m_psiRoot.hasChildren();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren( )
  {
    return m_psiRoot.getChildren();
  }

  /**
   * Always returns
   * 
   * <pre>
   * 
   *  psicompact://
   *  
   * </pre>
   * 
   * @see org.kalypso.repository.IRepository#getIdentifier()
   */
  public String getIdentifier( )
  {
    return "psicompact://";
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public final void reload( ) throws RepositoryException
  {
    try
    {
      final TreeMap nodes = new TreeMap();

      final PSICompactItem nodeMeasurements = buildStructure( nodes,
          PSICompact.TYPE_MEASUREMENT );
      final PSICompactItem nodeForecasts = buildStructure( nodes,
          PSICompact.TYPE_VALUE );

      if( nodeMeasurements != nodeForecasts )
      {
        System.out
            .println( "PSICompactRepository - Achtung: ungleiche Nodes bei Gemessene und Vorhergesagte." );

        m_psiRoot = new PSICompactItem( null, "Fehler...", "Fehler", null, 0 );
      }
      else
        m_psiRoot = nodeMeasurements;

      fireRepositoryStructureChanged();
    }
    catch( ECommException e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * @see org.kalypso.repository.IRepository#findItem(java.lang.String)
   */
  public IRepositoryItem findItem( final String id ) throws RepositoryException
  {
    final IRepositoryItem item = findItemRecursive( m_psiRoot, id );

    return item;
  }

  /**
   * Helper: finds using recursion. Returns null when not found
   * 
   * @param item
   * @param id
   * @return item or null if not found
   * 
   * @throws RepositoryException
   */
  private final IRepositoryItem findItemRecursive( final IRepositoryItem item,
      final String id ) throws RepositoryException
  {
    if( item.getIdentifier().equalsIgnoreCase( id ) )
      return item;

    final IRepositoryItem[] items = item.getChildren();
    for( int i = 0; i < items.length; i++ )
    {
      final IRepositoryItem item2 = findItemRecursive( items[i], id );

      if( item2 != null )
        return item2;
    }

    return null;
  }
}