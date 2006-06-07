package org.kalypso.model.wspm.ui.profil.view.table.swt;

import java.util.List;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Item;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;


/**
 * @author belger
 * 
 */
public class ProfilCellModifier implements ICellModifier
{
  private final TableViewer m_viewer;

  public ProfilCellModifier( final TableViewer viewer )
  {
    m_viewer = viewer;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    final IProfilEventManager pem = (IProfilEventManager) m_viewer.getInput();
    if( pem == null )
      return false;
      
    return propertyForID( pem.getProfil(), property ) != null;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    return String.format( ProfilLabelProvider.ENTRY_FORMAT, getValueAsDouble( element, property ) );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final IProfilEventManager pem = (IProfilEventManager) m_viewer.getInput();
    if( pem == null )
      return;

    final IProfilPoint point;
    if( element instanceof Item )
      point = (IProfilPoint) ((Item) element).getData();
    else if( element instanceof IProfilPoint )
      point = (IProfilPoint) element;
    else
      point = null;

    final POINT_PROPERTY col = propertyForID( pem.getProfil(), property );

    try
    {
      final double oldValue = point.getValueFor( col );
      final Double newValue = new Double( value.toString().replace( ',', '.' ) );

      if( Double.compare( oldValue, newValue ) != 0 )
      {
        final PointPropertyEdit[] profilChanges = new PointPropertyEdit[] { new PointPropertyEdit( point, col, newValue ) };
        final ProfilOperation operation = new ProfilOperation( "", pem, profilChanges, true );
        new ProfilOperationJob( operation ).schedule();
      }
    }
    catch( final Exception t )
    {
      // ignore, set null?
      t.printStackTrace();
    }
  }

  public static POINT_PROPERTY propertyForID( final IProfil profil, final String idstr )
  {
    final List<POINT_PROPERTY> columnKeys = profil.getPointProperties( true );
    for( final POINT_PROPERTY key : columnKeys )
    {
      final POINT_PROPERTY pointProperty = key;
      final String id = pointProperty.toString();
      if( id.equals( idstr ) )
        return pointProperty;
    }

    return null;
  }

  public double getValueAsDouble( final Object element, final String property )
  {
    final IProfilPoint row = (IProfilPoint) element;
    final IProfilEventManager pem = (IProfilEventManager) m_viewer.getInput();
    final POINT_PROPERTY col = propertyForID( pem.getProfil(), property );

    try
    {
      return row.getValueFor( col );
    }
    catch( final Exception e )
    {
      // should never happen
      e.printStackTrace();

      return Double.NaN;
    }
  }

}
