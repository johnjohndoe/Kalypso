package org.kalypso.ogc.gml.featureview.control;

import java.util.Properties;

import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Object;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.factory.ConfigurableCachableObjectFactory;
import org.kalypso.util.factory.FactoryException;

/**
 * Default-Factory für die CellEditoren Die Cell-Editoren werden gecached, d.h.
 * für jede Tabelle muss eine eigene Factory benutzt werden!
 * 
 * @author Belger
 */
public class DefaultFeatureControlFactory implements IFeatureControlFactory
{
  private final ConfigurableCachableObjectFactory m_factory;

  public DefaultFeatureControlFactory( final Properties props, final ClassLoader cl )
  {
    m_factory = new ConfigurableCachableObjectFactory( props, false, cl );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControlFactory#isDataTypeKnown(org.deegree.model.feature.FeatureTypeProperty)
   */
  public boolean isDataTypeKnown( final FeatureTypeProperty ftp )
  {
    return m_factory.isTypeKnown( ftp.getType() )
        || ftp.getType().startsWith( GM_Object.class.getPackage().getName() );
  }

  /**
   * @throws FactoryException
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControlFactory#createFeatureControl(org.eclipse.swt.widgets.Composite, int, org.deegree.model.feature.FeatureTypeProperty)
   */
  public AbstractFeatureControl createFeatureControl( final Composite parent, final int style, final FeatureTypeProperty ftp ) throws FactoryException
  {
    final String type = ftp.getType();

    final AbstractFeatureControl featureControl = (AbstractFeatureControl)m_factory.getObjectInstance( type,
          AbstractFeatureControl.class, new Object[] { parent, new Integer( style ) } );
//    else if( ftp.getType().startsWith( GM_Object.class.getPackage().getName() ) )
//      cellEditor = new GeometryFeatureCellEditor();

    return featureControl;
  }
}