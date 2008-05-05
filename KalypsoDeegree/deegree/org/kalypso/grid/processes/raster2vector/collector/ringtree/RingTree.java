package org.kalypso.grid.processes.raster2vector.collector.ringtree;

import java.util.ArrayList;
import java.util.Collection;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author belger
 */
public class RingTree
{
  public final RingTreeElement root = new RingTreeElement( null, -1, null );

  public Object walk( final RingTreeWalker rtw )
  {
    // für jedes child was machen
    walkInternal( rtw, root );

    return rtw.getResult();
  }

  private void walkInternal( final RingTreeWalker rtw, final RingTreeElement element )
  {
    if( element != root )
      rtw.operate( element );

    for( final RingTreeElement child : element.children() )
      walkInternal( rtw, child );
  }

  public void insertElement( final RingTreeElement rte )
  {
    // rekursiv durch den Baum suchen und das Element an der richtigen Stelle einbauen
    insertInternal( root, rte );
  }

  private void insertInternal( final RingTreeElement target, final RingTreeElement element )
  {
    // für jedes Kind checken, ob element drin liegt, falls ja dort einfügen
    final Coordinate c = element.innerCrd;

    for( final RingTreeElement child : target.children() )
    {
      if( child.pir.isInside( c ) && child.contains( element ) )
      {
        insertInternal( child, element );
        return;
      }
    }

    // wenns nirgends reinpasst schaun, welche children in das element gehören
    final Collection<RingTreeElement> removeElements = new ArrayList<RingTreeElement>();
    for( final RingTreeElement child : target.children() )
    {
      if( element.pir.isInside( child.innerCrd ) && element.contains( child ) )
      {
        removeElements.add( child );
        element.addChild( child );
      }
    }

    // und diese elemente löschen
    for( final RingTreeElement removeChild : removeElements )
      target.removeChild( removeChild );

    // und das element selbst hinzufügen
    target.addChild( element );
  }

}
