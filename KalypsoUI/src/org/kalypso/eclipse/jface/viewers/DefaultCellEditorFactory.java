package org.kalypso.eclipse.jface.viewers;

import java.util.Properties;

import org.deegree.model.feature.FeatureTypeProperty;
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
   * @see org.kalypso.eclipse.jface.viewers.ICellEditorFactory#createEditor(org.deegree.model.feature.FeatureTypeProperty, org.eclipse.swt.widgets.Composite, int)
   */
  public CellEditor createEditor( final FeatureTypeProperty ftp, final Composite parent,
      final int style ) throws FactoryException
  {
    final String type = ftp.getType();
    final CellEditor cellEditor = (CellEditor)m_factory.getObjectInstance( type, CellEditor.class,
        new Object[]
        { ftp.getName() } );

    cellEditor.setStyle( style );
    cellEditor.create( parent );

    return cellEditor;
  }

}