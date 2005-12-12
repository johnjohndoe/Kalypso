/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.dss.actiondelegates;

import java.net.URL;
import java.util.ArrayList;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.dss.KalypsoDSSPlugin;
import org.kalypso.dss.MeasuresConstants;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.featureTypeDialog.FeatureTypeSelectionDialog;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.LocatorImpl;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class AddGeometrieToMeasureActionDelegate implements IActionDelegate
{

  private IStructuredSelection m_selection;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    if( m_selection != null && action.isEnabled() )
    {
      //get geometry property from selected Feature
      final KalypsoFeatureThemeSelection featureSelection = (KalypsoFeatureThemeSelection)m_selection;
      final Feature firstFeature = FeatureSelectionHelper.getFirstFeature( featureSelection );
      if( firstFeature == null )
        return;
      final GM_Object[] geometryProperties = firstFeature.getGeometryProperties();
      if( geometryProperties.length < 1 && geometryProperties.length > 1 )
        return;
      final GM_Object geom = geometryProperties[0];
      //Load default workspace for measures from file
      final URL url = getClass().getResource( "../resources/v0.1/empty_measures_collection.gml" );
      CommandableWorkspace measures = null;
      try
      {
        measures = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( url ) );
        final Feature measureRootFeature = measures.getRootFeature();
        if( measureRootFeature == null )
          throw new SAXParseException( "Document root element is missing", new LocatorImpl() );

        final FeatureType selectedFT = firstFeature.getFeatureType();
        final FeatureType[] measureFT = measures.getFeatureTypes();
        final FeatureType[] possibleFt = getMatchingFT( selectedFT.getAllGeomteryProperties(), measureFT,
            MeasuresConstants.DSS_MEASURES_SUBST_GROUP, MeasuresConstants.DSS_MEASURES_NS );
        //choose the feature type to add the geometry property to
        final FeatureTypeSelectionDialog dialog = new FeatureTypeSelectionDialog(
            Display.getCurrent().getActiveShell(), possibleFt, SWT.SINGLE );
        final int status = dialog.open();
        FeatureType[] ftFromDialog = null;
        if( status == Window.OK )
        {
          ftFromDialog = dialog.getSelectedFeatureTypes();
          FeatureType ft = ftFromDialog[0];
          Feature newFeature = measures.createFeature( ft );

          IFeatureSelectionManager selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

          AddFeatureCommand command = new AddFeatureCommand( measures, ft, measureRootFeature,
              KalypsoDSSPlugin.MEASUER_MEMBER, 0, null, selectionManager );
          measures.postCommand( command );

        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
        final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "", e );

        // we are in the ui-thread so we get a shell here
        final Shell shell = Display.getCurrent().getActiveShell();
        if( shell != null )
          ErrorDialog.openError( shell, action.getText(), "Fehler beim Hinzufügen des Features", status );

      }

    }

  }

  /**
   * @param selectedFT
   * @param measureFT
   * @return
   */
  private FeatureType[] getMatchingFT( FeatureTypeProperty[] possibleFtp, FeatureType[] availableFT,
      String substitutionGroup, String nameSpace )
  {
    final ArrayList res = new ArrayList();
    for( int i = 0; i < availableFT.length; i++ )
    {
      FeatureType ft = availableFT[i];
      String substitutionGroup2 = ft.getSubstitutionGroup();
      if( substitutionGroup2 == null )
        continue;
      String ns = ft.getNamespace();
      if( !substitutionGroup2.equals( substitutionGroup ) && !ns.equals( nameSpace ) )
        continue;
      FeatureTypeProperty[] allGeomteryProperties = ft.getAllGeomteryProperties();
      for( int j = 0; j < allGeomteryProperties.length; j++ )
      {
        FeatureTypeProperty property = allGeomteryProperties[j];
        if( ArrayUtils.contains( allGeomteryProperties, property ) )
        {
          res.add( ft );
          break;
        }
      }
    }
    return (FeatureType[])res.toArray( new FeatureType[res.size()] );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    action.setEnabled( false );
    if( selection instanceof IStructuredSelection )
    {
      m_selection = (IStructuredSelection)selection;
      if( m_selection instanceof KalypsoFeatureThemeSelection )
      {
        KalypsoFeatureThemeSelection kalypsoFeatureThemeSelection = ( (KalypsoFeatureThemeSelection)m_selection );
        if( !m_selection.isEmpty() && m_selection.size() == 1 )
          action.setEnabled( true );
      }

    }

  }

}
