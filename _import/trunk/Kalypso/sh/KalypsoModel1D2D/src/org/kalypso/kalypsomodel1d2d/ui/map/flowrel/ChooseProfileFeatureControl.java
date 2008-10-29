/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.TreeSingleSelectionDialog;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBridgeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IWeirFlowRelation;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingBruecke;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.building.BuildingWehr;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;

/**
 * Lets the user choose a profile for a flow relation.
 * 
 * @author Gernot Belger
 */
public class ChooseProfileFeatureControl extends AbstractFeatureControl
{
  private Button m_button;

  public ChooseProfileFeatureControl( final Feature feature, final IPropertyType pt )
  {
    super( feature, pt );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, SWT.PUSH );

    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        showDialog( e.display.getActiveShell() );
      }
    } );

    updateControl();

    return m_button;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    final Feature linkedProfileFeature = getLinkedProfileFeature();

    if( linkedProfileFeature == null )
    {
      m_button.setText( "-kein Profil gew�hlt-" );
      m_button.setToolTipText( "W�hlen Sie ein Profil aus, um die Neuberechnung der Parameter und die Ergebnisauswertung in der Fl�che zu erm�glichen." );
    }
    else
    {
      m_button.setText( FeatureHelper.getAnnotationValue( linkedProfileFeature, IAnnotation.ANNO_LABEL ) );
      m_button.setToolTipText( FeatureHelper.getAnnotationValue( linkedProfileFeature.getOwner(), IAnnotation.ANNO_LABEL ) );
    }
  }

  protected void showDialog( final Shell shell )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return;

    final IFlowRelationship flowRel = (IFlowRelationship) feature.getAdapter( IFlowRelationship.class );
    Assert.isNotNull( flowRel );

    final Feature realProfileFeature = getLinkedProfileFeature();

    try
    {
      // TODO: add filter depending on relation type (bridge, weir, normal)

      // find network
      final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
      final ITerrainModel terrainModel = dataProvider.getModel( ITerrainModel.class );

      final GMLWorkspace root = terrainModel.getFeature().getWorkspace();
      final GMLContentProvider cp = new GMLContentProvider( false, false );
      cp.setRootPath( new GMLXPath( terrainModel.getRiverProfileNetworkCollection().getFeature() ) );
      final TreeSingleSelectionDialog treeSelectionDialog = new TreeSingleSelectionDialog( shell, root, cp, new GMLLabelProvider(), "W�hlen Sie ein Profil aus:" );

      if( realProfileFeature != null )
        treeSelectionDialog.setInitialSelections( new Object[] { realProfileFeature } );

      if( treeSelectionDialog.open() == Window.CANCEL )
        return;

      final Feature newProfileLink = (Feature) treeSelectionDialog.getResult()[0];

      // TODO: check, if the chosen profile is suitable for this relation
      final IProfil profile = ((IProfileFeature)newProfileLink).getProfil( );

      final String profileRef = "terrain.gml#" + newProfileLink.getId();
      if( flowRel instanceof ITeschkeFlowRelation )
      {
        final IRelationType pt = (IRelationType) flowRel.getFeature().getFeatureType().getProperty( ITeschkeFlowRelation.QNAME_PROP_PROFILE );
        final Feature newLinkFeature = new XLinkedFeature_Impl( feature, pt, pt.getTargetFeatureType(), profileRef, "", "", "", "", "" );
        fireFeatureChange( new ChangeFeatureCommand( flowRel.getFeature(), pt, newLinkFeature ) );
      }
      else if( flowRel instanceof IBuildingFlowRelation )
      {
        final IProfileObject[] profileObjects = profile.getProfileObjects();
        if( profileObjects.length == 0 )
          MessageDialog.openWarning( shell, "Profil zuweisen", "Das gew�hlte Profil enth�lt keine Bauwerke" );
        else
        {
          final IProfileObject building = profileObjects[0];

          if( flowRel instanceof IBridgeFlowRelation && !(building instanceof BuildingBruecke) )
            MessageDialog.openWarning( shell, "Profil zuweisen", "Das gew�hlte Profil enth�lt keine Br�cke" );
          else if( flowRel instanceof IWeirFlowRelation && !(building instanceof BuildingWehr) )
            MessageDialog.openWarning( shell, "Profil zuweisen", "Das gew�hlte Profil enth�lt kein Wehr" );
          else
          {
            final IRelationType pt = (IRelationType) flowRel.getFeature().getFeatureType().getProperty( IBuildingFlowRelation.QNAME_PROP_PROFILE );
            final Feature newLinkFeature = new XLinkedFeature_Impl( feature, pt, pt.getTargetFeatureType(), profileRef, "", "", "", "", "" );
            fireFeatureChange( new ChangeFeatureCommand( flowRel.getFeature(), pt, newLinkFeature ) );
          }
        }
      }

      // TODO: set name of flowrel according to profile or create a dummy name
      if( flowRel.getName().equals( "" ) )
        flowRel.setName( "" + profile.getStation() );

    }
    catch( final CoreException e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, "Profil zuweisen", "Profilauswahl konnte nicht ge�ffnet werden", e.getStatus() );
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( shell, "Profil zuweisen", "Profilauswahl konnte nicht ge�ffnet werden", status );
    }
  }

  private Feature getLinkedProfileFeature( )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;

    // set initial selection
    final Object profileRef = feature.getProperty( getFeatureTypeProperty() );
    final Feature profileFeature = FeatureHelper.getFeature( feature.getWorkspace(), profileRef );
    if( profileFeature == null )
      return null;

    final Feature realProfileFeature = ((XLinkedFeature_Impl) profileFeature).getFeature();
    return realProfileFeature;
  }

}
