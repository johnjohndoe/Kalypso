package org.kalypso.ogc.gml.table;

import java.text.Collator;
import java.util.Date;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

/**
 * @author belger
 */
public class LayerTableSorter extends ViewerSorter
{
  /** Die Features werden nach dieser Property sortiert */
  private String m_propertyName = null;
  
  private boolean m_bInverse = false;
  
  public LayerTableSorter()
  {
    super();
  }

  public LayerTableSorter( final Collator collator )
  {
    super( collator );
  }
  
  public final String getPropertyName()
  {
    return m_propertyName;
  }
  
  public final boolean isInverse()
  {
    return m_bInverse;
  }
  
  public final void setInverse( boolean bInverse )
  {
    m_bInverse = bInverse;
  }

  
  public final void setPropertyName( String propertyName )
  {
    m_propertyName = propertyName;
  }
  
  
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    final Feature kf1 = (Feature)e1;
    final Feature kf2 = (Feature)e2;
    
    final String propertyName = getPropertyName();
    final Object o1 = kf1.getProperty( propertyName );
    final Object o2 = kf2.getProperty( propertyName );
    
    final int sign = isInverse() ? -1 : 1;
    
    if( o1 instanceof String )
      return sign * ((String)o1).compareTo( o2 );
    else if( o1 instanceof Integer )
      return sign * ((Integer)o1).compareTo( o2 );
    else if( o1 instanceof Double )
      return sign * ((Double)o1).compareTo( o2 );
    else if( o1 instanceof Long )
      return sign * ((Long)o1).compareTo( o2 );
    else if( o1 instanceof Float )
      return sign * ((Float)o1).compareTo( o2 );
    else if( o1 instanceof Date )
      return sign * ((Date)o1).compareTo( o2 );
    
    return 0;
  }
  
  public boolean isSorterProperty( Object element, String property )
  {
    return true;
  }
}
