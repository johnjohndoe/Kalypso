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
package org.kalypso.kalypsomodel1d2d.schema.binding.result;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.ResultMeta;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Thomas Jung
 * 
 */
public class CalcUnitResultMeta extends ResultMeta implements ICalcUnitResultMeta
{
  public CalcUnitResultMeta( final Feature featureToBind )
  {
    super( featureToBind, ICalcUnitResultMeta.QNAME );
  }

  @Override
  public void setCalcStartTime( final Date startTime )
  {
    final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( startTime );
    getFeature().setProperty( QNAME_PROP_CALC_START_TIME, gregorianCalendar );
  }

  @Override
  public Date getCalcStartTime( )
  {
    return DateUtilities.toDate( (XMLGregorianCalendar) getFeature().getProperty( QNAME_PROP_CALC_START_TIME ) );
  }

  @Override
  public void setCalcEndTime( final Date endTime )
  {
    final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( endTime );
    getFeature().setProperty( QNAME_PROP_CALC_END_TIME, gregorianCalendar );
  }

  @Override
  public Date getCalcEndTime( )
  {
    return DateUtilities.toDate( (XMLGregorianCalendar) getFeature().getProperty( QNAME_PROP_CALC_END_TIME ) );
  }

  @Override
  public String getCalcUnit( )
  {
    return (String) getFeature().getProperty( QNAME_PROP_CALC_UNIT_ID );
  }

  @Override
  public void setCalcUnit( final String calcUnitID )
  {
    getFeature().setProperty( QNAME_PROP_CALC_UNIT_ID, calcUnitID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta#addStepResult()
   */
  @Override
  public IStepResultMeta addStepResult( )
  {
    return getChildren().addNew( IStepResultMeta.QNAME, IStepResultMeta.class );
  }

  @Override
  public boolean containsChildType( final DOCUMENTTYPE type )
  {
    final IFeatureWrapperCollection<IResultMeta> children = getChildren();
    for( IResultMeta child : children )
    {
      if( child instanceof IDocumentResultMeta )
      {
        IDocumentResultMeta docResult = (IDocumentResultMeta) child;
        if( docResult.getDocumentType() == type )
        {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta#getChild(org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE)
   */
  @Override
  public IDocumentResultMeta getDocument( DOCUMENTTYPE type )
  {
    final IFeatureWrapperCollection<IResultMeta> children = getChildren();
    for( IResultMeta child : children )
    {
      if( child instanceof IDocumentResultMeta )
      {
        IDocumentResultMeta docResult = (IDocumentResultMeta) child;
        if( docResult.getDocumentType() == type )
        {
          return docResult;
        }
      }
    }
    return null;

  }

  /**
   * returns all document children of the calc unit with specified type
   */
  @Override
  public IDocumentResultMeta[] getDocuments( DOCUMENTTYPE documenttype )
  {
    List<IResultMeta> documentList = new LinkedList<IResultMeta>();

    /* get all Node Documents */
    IFeatureWrapperCollection<IResultMeta> calcUnitChildren = getChildren();

    for( IResultMeta calcUnitChild : calcUnitChildren )
    {
      if( calcUnitChild instanceof IStepResultMeta )
      {
        IStepResultMeta stepResult = (IStepResultMeta) calcUnitChild;

        IFeatureWrapperCollection<IResultMeta> StepChildren = stepResult.getChildren();
        for( IResultMeta StepChild : StepChildren )
        {
          if( StepChild instanceof IDocumentResultMeta )
          {
            IDocumentResultMeta documentResult = (IDocumentResultMeta) StepChild;
            DOCUMENTTYPE docType = documentResult.getDocumentType();

            if( docType.equals( documenttype ) )
            {
              documentList.add( documentResult );
            }
          }
        }
      }
      else if( calcUnitChild instanceof IDocumentResultMeta )
      {
        IDocumentResultMeta documentResult = (IDocumentResultMeta) calcUnitChild;
        DOCUMENTTYPE docType = documentResult.getDocumentType();

        if( docType.equals( documenttype ) )
        {
          documentList.add( documentResult );
        }
      }
    }
    return documentList.toArray( new IDocumentResultMeta[documentList.size()] );
  }
}
