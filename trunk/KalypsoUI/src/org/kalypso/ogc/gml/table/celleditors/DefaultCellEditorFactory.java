package org.kalypso.ogc.gml.table.celleditors;

import java.util.Properties;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * Default-Factory f�r die CellEditoren
 * Die Cell-Editoren werden gecached, d.h. f�r jede Tabelle muss eine
 * eigene Factory benutzt werden!
 * 
 * @author Belger
 */
public class DefaultCellEditorFactory implements ICellEditorFactory
{
  private final ConfigurableCachableObjectFactory m_factory;

  public DefaultCellEditorFactory( final Properties props, final ClassLoader cl )
  {
    m_factory = new ConfigurableCachableObjectFactory( props, false, cl );
  }

  /**
   * @see org.kalypso.ogc.gml.table.celleditors.ICellEditorFactory#createEditor(org.deegree.model.feature.FeatureTypeProperty, org.eclipse.core.resources.IProject, org.eclipse.swt.widgets.Composite, int)
   */
  public AbstractFeatureCellEditor createEditor( final FeatureTypeProperty ftp, final IProject project, final Composite parent,
      final int style ) throws FactoryException
  {
    final String type = ftp.getType();
    
    final boolean isNew = !m_factory.hasInstance( type );
    final AbstractFeatureCellEditor cellEditor = (AbstractFeatureCellEditor)m_factory.getObjectInstance( type, AbstractFeatureCellEditor.class );

    if( isNew )
    {
      cellEditor.setPropertyName( ftp.getName() );
      cellEditor.setProject( project );
      cellEditor.setStyle( style );
      cellEditor.create( parent );
    }

    return cellEditor;
  }
}