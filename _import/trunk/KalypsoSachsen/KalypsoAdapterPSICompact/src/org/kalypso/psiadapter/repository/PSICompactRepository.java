package org.kalypso.psiadapter.repository;

import java.util.Hashtable;

import org.kalypso.java.util.Arrays;
import org.kalypso.psiadapter.util.ObjectInfoLengthComparator;
import org.kalypso.repository.AbstractRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.*;

/**
 * Spezifisches Repository für die PSICompact Struktur.
 * 
 * @author schlienger
 */
public class PSICompactRepository extends AbstractRepository
{
  private PSICompactItem m_psiRoot = null;

  public PSICompactRepository() throws RepositoryException
  {
    super( "PSICompact" );

    reload();
  }

  /**
   * Helper um die PSICompact ObjectInfos in einer Repository enabled Struktur
   * umzuwandeln.
   */
  private PSICompactItem buildStructure( Hashtable nodes, int valueType ) throws ECommException
  {
    ObjectInfo[] objInfos = PSICompactFactory.getConnection().getInfo( valueType );

    /*
     * TRICK #1:
     * 
     * Sortierung wichtig: damit werden die Leafs als erster bearbeitet und es
     * wird garantiert dass die Items die nur Ebenen sind keine Zeitreihen
     * subitems bekommen.
     * 
     * Die PSICompact Schnittstelle gibt nicht zurück, ob es sich um Ebenen oder
     * echte Timeseries handelt. Wir bekommen lediglich die Struktur.
     * 
     * Siehe auch TRICK #2 unten.
     */
    java.util.Arrays.sort( objInfos, new ObjectInfoLengthComparator() );

    PSICompactItem parent = null;

    for( int k = 0; k < objInfos.length; k++ )
    {
      final ObjectInfo info = objInfos[k];

      final String[] path = info.getId().split( "\\." );

      for( int i = 0; i < path.length; i++ )
      {
        final String nodeID = Arrays.implode( path, ".", 0, i );

        if( nodeID.length() == 0 )
          continue;

        if( nodes.containsKey( nodeID ) )
        {
          parent = (PSICompactItem)nodes.get( nodeID );
        }
        else
        {
          PSICompactItem n = null;

          // TRICK #2: nur die Leafs von der PSICompact Struktur sind Zeitreihen
          if( i == path.length - 1 )
            n = new PSICompactObservationItem( parent, path[i], info, valueType );
          else
            n = new PSICompactItem( parent, path[i], info );

          // gleich parent item aktualisieren (wird nicht von der Child gemacht,
          // deswegen hier)
          if( parent != null )
            parent.addChild( n );

          nodes.put( nodeID, n );

          parent = n;
        }
      }
    }

    while( parent.getParent() != null )
      parent = (PSICompactItem)parent.getParent();

    return parent;
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_psiRoot.hasChildren();
  }

  /**
   * @see org.kalypso.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return m_psiRoot.getChildren();
  }

  /**
   * @see org.kalypso.repository.IRepository#getIdentifier()
   */
  public String getIdentifier()
  {
    return "psicompact";
  }

  /**
   * @see org.kalypso.repository.IRepository#reload()
   */
  public void reload() throws RepositoryException
  {
    try
    {
      final Hashtable nodes = new Hashtable();

      final PSICompactItem nodeMeasurements = buildStructure( nodes, PSICompact.TYPE_MEASUREMENT );
      final PSICompactItem nodeForecasts = buildStructure( nodes, PSICompact.TYPE_VALUE );

      if( nodeMeasurements != nodeForecasts )
      {
        System.out
            .println( "PSICompactRepository - Achtung: ungleiche Nodes bei Gemessene und Vorhergesagte." );

        m_psiRoot = new PSICompactItem( null, "psi", null );
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

    if( item == null )
      throw new RepositoryException( "Item <" + id + "> could not be found!" );

    return item;
  }

  /**
   * Helper: finds using recursion. Returns null when not found
   */
  private IRepositoryItem findItemRecursive( final IRepositoryItem item, final String id )
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