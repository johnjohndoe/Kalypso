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
package org.kalypso.kalypsomodel1d2d.ui.CalculationUnitView;

import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 *
 */
public class FindInvalidElements
{

  private ICalculationUnit calculationUnit;
  private IFeatureWrapperCollection featureList;
  private IFE1D2DElement[] oldElements;
  private IElement2D element2D;
  private IFE1D2DEdge[] edges;
  private int count = 0;
  private IElement2D otherElement;
  public FindInvalidElements()
  {
    
  }
  
  public FindInvalidElements(ICalculationUnit calculationUnit)
  {
    this.calculationUnit = calculationUnit;
  }
  
  //@TODO Should return a boolean with IProblem if the calculation Unit has Bad Structure
  public void FindElements()
  {
    featureList = calculationUnit.getElements();
    
    oldElements = (IFE1D2DElement[]) featureList.toArray( new IFE1D2DElement[]{} );
    for ( IFE1D2DElement feat: oldElements)
    {      
      if (feat instanceof IElement2D)
      {
         boolean foundBLine = false;
         element2D = (IElement2D) feat;
         edges = (IFE1D2DEdge[])element2D.getEdges().toArray( new IFE1D2DEdge[]{} );
         for (IFE1D2DEdge edge:edges)
         {
           List<Feature> collectElements = edge.getContainers().getWrappedList();
           for (Feature thisElement : collectElements)
           {
             if (thisElement instanceof IBoundaryLine)
             {
               foundBLine = true;
             }
           }
           
         }
         
         if (!foundBLine)
         {
         for (IFE1D2DEdge edge:edges)
         {
            List<IElement2D> collectElements = edge.getContainers().getWrappedList();             
            for (IElement2D element :collectElements)
            {
              if (!element.equals( element2D ))
              {
                otherElement = element; 
              }
            }
            if (collectElements.contains( element2D )&& featureList.contains( otherElement ))
            {
              count++;             
            }
         }
         }
         
         
      }      
    }
  }
  
}
