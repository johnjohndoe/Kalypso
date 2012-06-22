package org.kalypso.ui.rrm.internal.simulations;

import java.util.Properties;

import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.control.IExtensionsFeatureControlFactory2;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * The simulation calculation feature control factory.
 * 
 * @author Holger Albert
 */
public class SimulationCalculationFeatureControlFactory implements IExtensionsFeatureControlFactory2
{
  @Override
  public IFeatureControl createFeatureControl( final FormToolkit toolkit, final Feature feature, final IPropertyType pt, final Properties arguments )
  {
    return new SimulationCalculationFeatureControl( feature, pt );
  }
}