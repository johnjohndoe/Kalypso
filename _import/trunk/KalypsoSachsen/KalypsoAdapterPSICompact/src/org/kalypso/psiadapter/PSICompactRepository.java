package org.kalypso.psiadapter;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import org.kalypso.java.util.Arrays;
import org.kalypso.util.repository.AbstractRepository;
import org.kalypso.util.repository.IRepositoryItem;
import org.kalypso.util.repository.RepositoryException;

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
				System.out.println("Achtung: ungleiche Nodes bei Gemessene und Vorhergesagte.");
				
				m_psiRoot = new PSICompactItem( null, "psi" ); //, new ObjectInfo(), PSICompact.TYPE_UNDEF );
			}
			else
				m_psiRoot = nodeMeasurements;
		}
		catch( ECommException e )
		{
			throw new RepositoryException( e );
		}
	}

	private Map buildMap(ObjectInfo[] mes)
	{
		Map m = new Hashtable();
		
		for (int i = 0; i < mes.length; i++)
			m.put(mes[i].getId(), mes[i]);
		
		return m;
	}

	private PSICompactItem buildStructure( Hashtable nodes, int valueType ) throws ECommException
	{
		ObjectInfo[] objInfos = PSICompactFactory.getConnection().getInfo( valueType );
		Map map = buildMap( objInfos );
		
		Iterator it = map.values().iterator();
		PSICompactItem parent = null;

		while( it.hasNext() )
		{
			ObjectInfo info = (ObjectInfo) it.next();
			
			String[] path = info.getId().split("\\.");
	
			for (int i = 0; i < path.length; i++)
			{
				String nodeID = Arrays.implode(path, ".", 0, i);
				
				if( nodeID.length() == 0 )
					continue;
				
				if( nodes.containsKey( nodeID ) )
				{
					parent = (PSICompactItem) nodes.get( nodeID );
				}
				else			
				{
          PSICompactItem n = new PSICompactItem( parent, path[i] ); // info, valueType );
	
					if( parent != null )
						parent.addChild(n);
					
					nodes.put( nodeID, n );
					
					parent = n;
				}	
			}
		}
		
		while( parent.getParent() != null )
			parent = (PSICompactItem) parent.getParent();
			
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
}
