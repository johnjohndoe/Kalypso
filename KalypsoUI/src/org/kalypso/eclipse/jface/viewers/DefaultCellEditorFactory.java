package org.kalypso.eclipse.jface.viewers;

import java.util.Properties;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * @author gernot
 */
public class DefaultCellEditorFactory implements ICellEditorFactory
{
  private final ConfigurableCachableObjectFactory m_factory;

  /**
   * @param props
   *  
   */

  public DefaultCellEditorFactory( final Properties props, final ClassLoader cl )
  {
    m_factory = new ConfigurableCachableObjectFactory( props, false, cl );
  }

  /**
   * @throws FactoryException
   * @see org.kalypso.eclipse.jface.viewers.ICellEditorFactory#createEditor(java.lang.String, org.eclipse.swt.widgets.Composite, int)
   */
  public CellEditor createEditor( final String type, final Composite parent, final int style )
      throws FactoryException
  {
    final CellEditor cellEditor = (CellEditor)m_factory.getObjectInstance( type, CellEditor.class );

    cellEditor.setStyle( style );
    cellEditor.create( parent );

    return cellEditor;
  }

}