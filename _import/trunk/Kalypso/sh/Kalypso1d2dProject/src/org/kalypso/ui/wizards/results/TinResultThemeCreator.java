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
package org.kalypso.ui.wizards.results;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.StepResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public class TinResultThemeCreator extends AbstractThemeCreator
{
  private final List<ResultAddLayerCommandData> m_resultLayerCommandData = new LinkedList<ResultAddLayerCommandData>();

  private IDocumentResultMeta m_documentResult = null;

  private boolean m_lineButtonChecked = true;

  private boolean m_polyButtonChecked = true;

  public TinResultThemeCreator( IDocumentResultMeta documentResult )
  {
    m_documentResult = documentResult;
  }

  @Override
  public Composite createControl( Group parent )
  {

    // check if it is a TIN

    /* create control with buttons */
    final Composite buttonComp = new Composite( parent, SWT.BORDER );
    buttonComp.setLayout( new GridLayout() );

    final Button lineButton = new Button( buttonComp, SWT.CHECK );
    lineButton.setText( "Darstellung als Isolinien" );
    lineButton.setToolTipText( "Bei Auswahl wird das Ergebnis mittels Isolinien in der Karte dargestellt." );
    lineButton.setSelection( m_lineButtonChecked );

    final Button polyButton = new Button( buttonComp, SWT.CHECK );
    polyButton.setText( "Darstellung als Isofl‰chen" );
    polyButton.setToolTipText( "Bei Auswahl wird das Ergebnis mittels Isofl‰chen in der Karte dargestellt." );
    polyButton.setSelection( m_polyButtonChecked );

    createThemeCommandData();

    lineButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // update HashMap / Button Selection
        if( lineButton.getSelection() == true )
        {
          m_lineButtonChecked = true;
          createThemeCommandData();
        }
        else
          m_lineButtonChecked = false;
      }
    } );

    polyButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // update HashMap / Button Selection
        if( polyButton.getSelection() == true )
        {
          m_polyButtonChecked = true;
          createThemeCommandData();
        }
        else
          m_polyButtonChecked = false;
      }

    } );
    return buttonComp;

  }

  public void createThemeCommandData( )
  {
    /* init */
    m_resultLayerCommandData.clear();

    /* fill */

    /* get infos about time step */
    StepResultMeta stepResultMeta = (StepResultMeta) m_documentResult.getParent();

    /* get infos about calc unit */
    IResultMeta calcUnitMeta = m_documentResult.getParent().getParent();

    String style = null;
    String themeName = null;
    String resultType = "gml";

    String styleLinkType = "sld";
    String styleType = "simple";
    String featurePath = "";
    String source = "../" + m_documentResult.getFullPath().toPortableString();

    /* tin style path */
    String styleLocation = "../" + calcUnitMeta.getFullPath().append( "Styles" ).append( "tinStyles.sld" ).toPortableString();

    if( m_polyButtonChecked == true )
    {
      /* Iso-Areas */
      style = "tin" + getTypeName() + "PolygonStyle";
      themeName = m_documentResult.getName() + " (Isofl‰chen), " + calcUnitMeta.getName();

      m_resultLayerCommandData.add( new ResultAddLayerCommandData( themeName, resultType, featurePath, source, style, styleLocation, styleLinkType, styleType, calcUnitMeta, stepResultMeta ) );
    }

    if( m_lineButtonChecked == true )
    {
      /* Iso-Lines */
      style = "tin" + getTypeName() + "LineStyle";
      themeName = m_documentResult.getName() + " (Isolinien), " + calcUnitMeta.getName();

      m_resultLayerCommandData.add( new ResultAddLayerCommandData( themeName, resultType, featurePath, source, style, styleLocation, styleLinkType, styleType, calcUnitMeta, stepResultMeta ) );
    }
  }

  private String getTypeName( )
  {
    DOCUMENTTYPE documentType = m_documentResult.getDocumentType();

    switch( documentType )
    {
      case tinDepth:
        return "DEPTH";

      case tinShearStress:
        return "SHEARSTRESS";

      case tinVelo:
        return "VELOCITY";

      case tinWsp:
        return "WATERLEVEL";

      default:
        return "WATERLEVEL";
    }

  }

  /**
   * @see org.kalypso.ui.wizards.results.IResultThemeCreator#createThemeCommandData(org.kalypso.template.gismapview.Gismapview)
   */
  @Override
  public ResultAddLayerCommandData[] getThemeCommandData( )
  {
    if( m_resultLayerCommandData != null )
      return m_resultLayerCommandData.toArray( new ResultAddLayerCommandData[m_resultLayerCommandData.size()] );
    else
      return null;
  }
}
