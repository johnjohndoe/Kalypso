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
package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import javax.xml.namespace.QName;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.ogc.sensor.view.ObservationViewerDialog;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kuepfer
 */
public class ZmlInlineFeatureDialog implements IFeatureDialog
{
  private Feature m_feature;

  private IPropertyType m_ftp;

  private FeatureChange m_change = null;

  private static ITypeHandler m_typeHandler;

  public ZmlInlineFeatureDialog( Feature feature, IPropertyType ftp, ITypeHandler handler )
  {

    m_feature = feature;
    m_ftp = ftp;
    m_typeHandler = handler;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( final Shell shell )
  {
    ZmlInlineTypeHandler inlineTypeHandler = null;
    ObservationViewerDialog dialog = null;
    // Dies ist ein hack!
    // TODO Definition eines Extension points für den ObservationViewerDialog damit für jeden TypeHandler
    // der Dialog configuriert werden kann, oder ist dies hier anders gedacht ?? CK
    // Extension Point müsste z.B. im eine Methode wie newObservation(Shell parent) im Interface haben
    // die eine IObservation zurück gibt
    if( m_typeHandler instanceof ZmlInlineTypeHandler )
    {
      inlineTypeHandler = (ZmlInlineTypeHandler) m_typeHandler;
      final QName typeName = inlineTypeHandler.getTypeName();

      if( !(typeName.getLocalPart().equals( "ZmlInlineIdealKcWtLaiType" )) )
        dialog = new ObservationViewerDialog( shell, false, true, true, ObservationViewerDialog.BUTTON_NEW | ObservationViewerDialog.BUTTON_REMOVE | ObservationViewerDialog.BUTTON_EXEL_IMPORT
            | ObservationViewerDialog.BUTTON_EXEL_EXPORT, inlineTypeHandler.getAxisTypes() );
      else
      {
        dialog = new ObservationViewerDialog( shell, false, true, true, ObservationViewerDialog.BUTTON_NEW_IDEAL_LANDUSE | ObservationViewerDialog.BUTTON_REMOVE
            | ObservationViewerDialog.BUTTON_EXEL_IMPORT | ObservationViewerDialog.BUTTON_EXEL_EXPORT, inlineTypeHandler.getAxisTypes() );
      }
    }

    final Object o = m_feature.getProperty( m_ftp );
    dialog.setInput( o );
    int open = dialog.open();
    FeatureChange fChange = null;
    if( open == Window.OK )
    {
      final Object newValue = dialog.getInput();
      fChange = new FeatureChange( m_feature, m_ftp, newValue );
    }
    // TODO: implement real cancel
    m_change = fChange;
    return open;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection<FeatureChange> c )
  {
    if( m_change != null )
      c.add( m_change );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel( )
  {
    return "Diagramm...";
  }
}
