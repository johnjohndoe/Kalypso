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
package org.kalypso.ui.editor.actions;

import java.util.Map;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.FindLinkedFeatureVisitor;

/**
 * Uitls for handling feature lists in the table feature control.
 * 
 * @author Holger Albert
 */
public class TableFeatureControlUtils
{
  /**
   * The constructor.
   */
  private TableFeatureControlUtils( )
  {
  }

  /**
   * This function will build the command for deleting features from the list.
   * 
   * @param allFeatures
   *            The features, that should be deleted.
   * @param shell
   *            The shell.
   */
  public static DeleteFeatureCommand deleteFeaturesFromSelection( EasyFeatureWrapper[] allFeatures, Shell shell )
  {
    if( allFeatures.length > 0 )
    {
      String[] gmlIds = new String[allFeatures.length];
      for( int i = 0; i < gmlIds.length; i++ )
        gmlIds[i] = allFeatures[i].getFeature().getId();

      /* Work with the first workspace, normally all features in this context should live in the same workspace. */
      /* Furthermore, it is not relevant, in which workspace the command is processed. */
      CommandableWorkspace workspace = allFeatures[0].getWorkspace();

      /* Find features with links to the removed features and display a warning message. */
      FindLinkedFeatureVisitor visitor = new FindLinkedFeatureVisitor( gmlIds );
      workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      final Map<Feature, Set<IRelationType>> linkedFeatures = visitor.getLinkedFeatures();
      if( linkedFeatures.size() > 0 )
      {
        String msg;
        if( allFeatures.length == 1 )
          msg = "Das zu löschende Feature wird von den folgenden Features benutzt (gelinkt).\nSoll trotzdem gelöscht werden?";
        else
          msg = "Die zu löschenden Features werden von den folgenden Features benutzt (gelinkt).\n Soll trotzdem gelöscht werden?";

        MessageDialog dialog = new MessageDialog( shell, "Features löschen", null, msg, MessageDialog.WARNING, new String[] { IDialogConstants.OK_LABEL, IDialogConstants.CANCEL_LABEL }, 0 )
        {
          /**
           * @see org.eclipse.jface.dialogs.MessageDialog#createCustomArea(org.eclipse.swt.widgets.Composite)
           */
          @Override
          protected Control createCustomArea( Composite dialogParent )
          {
            TableViewer viewer = new TableViewer( dialogParent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.HIDE_SELECTION );
            viewer.setContentProvider( new ArrayContentProvider() );
            viewer.setLabelProvider( new GMLLabelProvider()
            {
              /**
               * @see org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2#getText(java.lang.Object)
               */
              @Override
              public String getText( Object element )
              {
                if( element instanceof Feature )
                  return FeatureHelper.getAnnotationValue( (Feature) element, IAnnotation.ANNO_NAME ) + ": <" + FeatureHelper.getAnnotationValue( (Feature) element, IAnnotation.ANNO_LABEL ) + ">";

                return super.getText( element );
              }
            } );
            viewer.setInput( linkedFeatures.keySet() );

            viewer.getTable();

            Control control = viewer.getControl();
            GridData gridData = new GridData( SWT.FILL, SWT.FILL, true, true );
            gridData.minimumHeight = 200;
            gridData.widthHint = 200;
            gridData.heightHint = 200;
            control.setLayoutData( gridData );

            return control;
          }
        };

        if( dialog.open() == Window.CANCEL )
          return null;
      }

      DeleteFeatureCommand command = new DeleteFeatureCommand( allFeatures );
      return command;
    }

    return null;
  }
}