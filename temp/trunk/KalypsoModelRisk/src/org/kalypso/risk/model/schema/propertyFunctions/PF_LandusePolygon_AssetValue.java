/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.risk.model.schema.propertyFunctions;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.risk.model.schema.binding.IAssetValueClass;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic
 */
public class PF_LandusePolygon_AssetValue extends FeaturePropertyFunction
{
  private final static QName XLINKED_LANDUSE_CLS = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "landuseClassMember" );

  private List<IAssetValueClass> m_assetValueClassesList = null;

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
    // initialization of m_assetValueClassesList here produces a deadlock
  }

  private void loadAssetValueClassesList( )
  {
    if( m_assetValueClassesList != null )
      return;
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final SzenarioDataProvider scenarioDataProvider = (SzenarioDataProvider) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    try
    {
      final IRasterizationControlModel model = scenarioDataProvider.getModel( IRasterizationControlModel.class );
      m_assetValueClassesList = model.getAssetValueClassesList();
    }
    catch( CoreException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @SuppressWarnings("unchecked")
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    loadAssetValueClassesList();
    final XLinkedFeature_Impl landuseClass = (XLinkedFeature_Impl) feature.getProperty( XLINKED_LANDUSE_CLS );
    if( landuseClass == null )
      return Double.NaN;
    else
    {
      // TODO For the moment, administration units are ignored; consider using administration units

      final String landuseClassGmlID = landuseClass.getFeatureId();
      for( final IAssetValueClass assetValueClass : m_assetValueClassesList )
      {
        if( assetValueClass.getLanduseClassGmlID().equals( landuseClassGmlID ) )
          return assetValueClass.getAssetValue();
      }
      return Double.NaN;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    return null;
  }

}
