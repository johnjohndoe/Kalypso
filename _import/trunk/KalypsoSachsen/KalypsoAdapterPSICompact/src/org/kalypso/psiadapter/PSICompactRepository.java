package org.kalypso.psiadapter;

import java.util.Comparator;
import java.util.Hashtable;

import org.kalypso.java.util.Arrays;
import org.kalypso.util.repository.AbstractRepository;
import org.kalypso.util.repository.IRepositoryItem;
import org.kalypso.util.repository.RepositoryException;

import de.psi.go.lhwz.ECommException;
import de.psi.go.lhwz.PSICompact;
import de.psi.go.lhwz.PSICompact.*;

/**
 * Spezifisches Repository f�r die PSICompact Struktur.
 * 
 * @author schlienger
 */
public class PSICompactRepository extends AbstractRepository
{
  private final PSICompactItem m_psiRoot;

  public PSICompactRepository( String location ) throws RepositoryException
  {
    super( location );

    try
    {
      Hashtable nodes = new Hashtable();

      PSICompactItem nodeMeasurements = buildStructure( nodes, PSICompact.TYPE_MEASUREMENT );
      PSICompactItem nodeForecasts = buildStructure( nodes, PSICompact.TYPE_VALUE );

      if( nodeMeasurements != nodeForecasts )
      {
        System.out.println( "Achtung: ungleiche Nodes bei Gemessene und Vorhergesagte." );

        m_psiRoot = new PSICompactItem( null, "psi", new ObjectInfo("psi", "psi root") );
      }
      else
        m_psiRoot = nodeMeasurements;
    }
    catch( ECommException e )
    {
      throw new RepositoryException( e );
    }
  }

  /**
   * Helper um die PSICompact ObjectInfos in einer Repository enabled Struktur umzuwandeln. 
   */
  private PSICompactItem buildStructure( Hashtable nodes, int valueType ) throws ECommException
  {
    ObjectInfo[] objInfos = PSICompactFactory.getConnection().getInfo( valueType );
    
    /* 
     * TRICK #1:
     * 
     * Sortierung wichtig: damit werden die Leafs als erster bearbeitet
     * und es wird garantiert dass die Items die nur Ebenen sind keine
     * Zeitreihen subitems bekommen.
     * 
     * Die PSICompact Schnittstelle gibt nicht zur�ck, ob es sich um
     * Ebenen oder echte Timeseries handelt. Wir bekommen lediglich
     * die Struktur.
     * 
     * Siehe auch TRICK #2 unten.  
     */
    java.util.Arrays.sort( objInfos, new ObjectInfoComparator() );    
    
    PSICompactItem parent = null;

    for( int k = 0; k < objInfos.length; k++ )
    {
      ObjectInfo info = objInfos[k];

      String[] path = info.getId().split( "\\." );

      for( int i = 0; i < path.length; i++ )
      {
        String nodeID = Arrays.implode( path, ".", 0, i );

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

          // gleich parent item aktualisieren (wird nicht von der Child gemacht, deswegen hier)
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
   * @see org.kalypso.util.repository.IRepositoryItem#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_psiRoot.hasChildren();
  }

  /**
   * @see org.kalypso.util.repository.IRepositoryItem#getChildren()
   */
  public IRepositoryItem[] getChildren()
  {
    return m_psiRoot.getChildren();
  }
  
  /**
   * Helper class. Wird benutzt um ObjectInfo zu sortieren. Siehe TRICKs in der Hauptklasse.
   * 
   * @author schlienger
   */
  private static final class ObjectInfoComparator implements Comparator
  {
    /**
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare( Object arg0, Object arg1 )
    {
      int l1 = ((ObjectInfo)arg0).getId().length();
      int l2 = ((ObjectInfo)arg1).getId().length();
      
      if( l1 > l2 )
        return -1;
      else if( l1 < l2 )
        return 1;
      else     
        return 0;
    }
  }
}
