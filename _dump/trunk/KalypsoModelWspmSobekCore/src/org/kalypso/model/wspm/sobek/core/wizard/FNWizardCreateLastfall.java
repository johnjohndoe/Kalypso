/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.sobek.core.wizard;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.sobek.core.interfaces.IModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.utils.AtomarAddFeatureCommand;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageCreateLastfall;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuch
 */
public class FNWizardCreateLastfall extends Wizard implements INewWizard
{

  private PageCreateLastfall m_page;

  private final IModelMember m_modelBuilder;

  /**
   * @param modelBuilder
   *            instance of sobek model gml notation
   */
  public FNWizardCreateLastfall( final IModelMember modelBuilder )
  {
    m_modelBuilder = modelBuilder;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Feature model = m_modelBuilder.getFeature();

    final IRelationType prop = (IRelationType) model.getFeatureType().getProperty( ISobekConstants.QN_HYDRAULIC_LASTFALL_MEMBER );
    final IFeatureType targetType = prop.getTargetFeatureType();
    final IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

    final Map<IPropertyType, Object> atValues = new HashMap<IPropertyType, Object>();
    atValues.put( targetType.getProperty( ISobekConstants.QN_HYDRAULIC_NAME ), m_page.getLastfallName() );
    atValues.put( targetType.getProperty( ISobekConstants.QN_HYDRAULIC_DESCRIPTION ), m_page.getLastfallDescription() );

    final CommandableWorkspace workspace = FeatureUtils.getWorkspace( model );

    final AtomarAddFeatureCommand command = new AtomarAddFeatureCommand( workspace, targetType, model, prop, -1, atValues, selectionManager );
    try
    {
      workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return false;
    }

    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_page = new PageCreateLastfall();
    addPage( m_page );
  }

}
