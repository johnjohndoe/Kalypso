/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
 
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 
Contact:
 
Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de
 
Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de
 
 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.graphics.legend;

import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.deegree.graphics.legend.*;
import org.deegree.graphics.sld.*;
import org.deegree.services.wfs.filterencoding.*;

import org.deegree_impl.services.wfs.filterencoding.*;
import org.deegree_impl.tools.Debug;

/**
 *
 *
 * @version $Revision$
 * @author $author$
 */
public class LegendFactory {

	private String label = "";
	private String legendtitle = "";
	private String legendtitlefilterproperty = "";

	/**
	 * creates a <tt>LegendElement</tt> from a SLD <tt>Style</tt>. Depending on
	 * the <tt>Style</tt> the returned <tt>LegendElement</tt> may is a
	 * <tt>LegendElementCollection</tt>.
	 *
	 * @return <tt>LegendElement</tt>
	 */
	public LegendElement createLegendElement(Style style, int width, int height, String title) throws LegendException {
		Debug.debugMethodBegin();
		setLegendTitle(title);

		if (style instanceof UserStyle) {

			LegendElement le = null;
			Rule[] rules = null;
			Filter f = null;
			String propertyname = "";

			FeatureTypeStyle[] fts = ((UserStyle)style).getFeatureTypeStyles();
			LegendElementCollection lec = createLegendElementCollection();

			for (int a = 0; a < fts.length; a++) {
				rules = fts[a].getRules();

				for (int b = 0; b < rules.length; b++) {

					if (rules[b].getFilter() != null) {
						f = rules[b].getFilter();
						propertyname = getPropertyNameFromFilter(f);                       
                        le = new LegendElement_Impl(new Rule[] {rules[b]}, propertyname, 0, 4, true, width, height);
						lec.addLegendElement( le );
					} else {
						// TODO null oder ""
						// le = new LegendElement_Impl(rules, fts[a].getName(), 0, 4, true, width, height);
						le = new LegendElement_Impl(rules, "", 0, 4, true, width, height);
					}
				}
			}

			if (lec.getSize() >= 1) {
				if (getLegendTitle() != null && getLegendTitle().length() > 0) {
                    if ( getLegendTitleFilterProperty() == null || 
                         getLegendTitleFilterProperty().trim().equals("")) {
                         lec.setTitle( getLegendTitle() );
                    }  else {
                         lec.setTitle( getLegendTitle() + " (" + getLegendTitleFilterProperty() + ")");
                    }
				} else {
					lec.setTitle(getLegendTitleFilterProperty());
				}
				Debug.debugMethodEnd();
				return lec;
			} else {
				Debug.debugMethodEnd();
				return le;
			}
		} else {
			throw new LegendException("LegendFactory: Error in creating the LegendElement:\n" + "Given style is not a valid UserStyle.");
		}
	}
	

	/**
	 * creates an empty <tt>LegendElementCollection</tt>
	 *
	 * @return <tt>LegendElementCollection</tt>
	 */
	public LegendElementCollection createLegendElementCollection() {
		Debug.debugMethodBegin("LegendFactory", "createLegendElementCollection()");
		Debug.debugMethodEnd();
		return new LegendElementCollection_Impl();
	}

	/**
	 * creates a <tt>LegendElementCollection</tt> and fills it with the passed
	 * <tt>LegendElement</tt>s.
	 *
	 * @return <tt>LegendElementCollection</tt>
	 */
	public LegendElementCollection createLegendElementCollection(LegendElement[] legendElements) {
		Debug.debugMethodBegin("LegendFactory", "createLegendElementCollection(LegendElement[])");
		LegendElementCollection lec = new LegendElementCollection_Impl();

		for (int i = 0; i < legendElements.length; i++) {
			lec.addLegendElement(legendElements[i]);
		}
		Debug.debugMethodEnd();
		return lec;
	}

	/**
	 *
	 *
	 * @return
	 */
	public static LegendView createLegendView(LegendElementCollection[] collection) {
		Debug.debugMethodBegin();
		Debug.debugMethodEnd();
		return null;
	}
	
	
	/**
	 * 
	 * @param sld
	 * @param width
	 * @param height
	 * @return
	 * @throws LegendException
	 */
	public BufferedImage[] createAllThumbnails(StyledLayerDescriptor sld, int width, int height) throws LegendException {
		Debug.debugMethodBegin( this, "createAllThumbnails" );
        
		ArrayList list = new ArrayList();
        
		org.deegree.graphics.sld.Layer[] nl = sld.getNamedLayers();
		for (int i = 0; i < nl.length; i++) {
			Style[] styles = nl[i].getStyles();
			for (int j = 0; j < styles.length; j++) {
				if ( styles[j] instanceof UserStyle ) {
					list.add( styles[j] );
				}
			}
		}
        
		nl = sld.getUserLayers();
		for (int i = 0; i < nl.length; i++) {
			Style[] styles = nl[i].getStyles();
			for (int j = 0; j < styles.length; j++) {
				if ( styles[j] instanceof UserStyle ) {
					list.add( styles[j] );
				}
			}
		}
        
        LegendElement le = null;
        BufferedImage bi_temp = null; // just temporary
        BufferedImage[] buffi = new BufferedImage[list.size()]; // @return
        
		for (int i = 0; i < list.size(); i++) {
			Style style = (Style)list.get(i);
			String name = style.getName();
			name = name.replace(':', '_');
			System.out.println("creating: " + name);
			le = createLegendElement( style, width, height, "" );
			bi_temp = le.exportAsImage();
			buffi[i] = bi_temp;
		}
        
		Debug.debugMethodEnd();
		return buffi;
	}
	
	
	/**
	 * gets the property-names for creating the legend text
	 */
	private String getPropertyNameFromFilter(Filter filter) throws LegendException {

		Debug.debugMethodBegin("LegendFactory", "getPropertyNameFromFilter()");

		String legendlabel = "";

		ComplexFilter cf = (ComplexFilter)filter;

		// System.out.println("Name der Operation: " + cf.getOperation().getOperatorName()); //DEBUG

		Operation operation = cf.getOperation();

		// determines the operation
		if (operation instanceof PropertyIsCOMPOperation) {
			PropertyIsCOMPOperation pCOMPo = (PropertyIsCOMPOperation)operation;
			legendlabel = getOperationString(pCOMPo.getOperatorId());

			// gets the PropertyName of the operation for creating a legendtitle
			if (pCOMPo.getFirstExpression() instanceof PropertyName) {

				PropertyName propertyname = (PropertyName)pCOMPo.getFirstExpression();
				setLegendTitleFilterProperty(propertyname.getValue());

			} else {
				throw new LegendException("LegendElement_Impl: An error occured "+
                                          "during the parsing of the Filter in the SLD." + 
                                          "First Operation Expression is not of type Literal");
			}

			// gets the Literal of the operation
			if (pCOMPo.getSecondExpression() instanceof Literal) {
				Literal literal = (Literal)pCOMPo.getSecondExpression();
				legendlabel += literal.getValue();
			} else {
				throw new LegendException("LegendElement_Impl: An error occured "+
                                          "during the parsing of the Filter in the SLD." + 
                                          "Second Operation Expression is not of type Literal");
			}

		} else if (operation instanceof SpatialOperation) {

			SpatialOperation spatop = (SpatialOperation)operation;
			
			legendlabel = "spatial operation" + spatop;
		} else if (operation instanceof PropertyIsLikeOperation) {
			
			PropertyIsLikeOperation prilop = (PropertyIsLikeOperation)operation;
			
			legendlabel = prilop.getPropertyName().getValue() + getOperationString(prilop.getOperatorId()) + prilop.getLiteral().getValue();
			
		} else {
			// TODO implement other filter-operations
			throw new LegendException("Filter-Operation <" + operation.getOperatorName() + 
                                      "> is no PropertyIsCOMPOperation.");
		}

		Debug.debugMethodEnd();
		return legendlabel;

	}

	/**
     * 
     * @param operationID
     * @return
     */
	private String getOperationString(int operationID) {
		Debug.debugMethodBegin("LegendElement_Impl", "getOperationString(int)");
		String operationString = "";
		// System.out.println("OperationID: " + operationID);
		switch (operationID) {
			case OperationDefines.PROPERTYISEQUALTO :
				operationString = "= ";
				break;
			case OperationDefines.PROPERTYISLESSTHAN :
				operationString = "< ";
				break;
			case OperationDefines.PROPERTYISGREATERTHAN :
				operationString = "> ";
				break;
			case OperationDefines.PROPERTYISLESSTHANOREQUALTO :
				operationString = "<= ";
				break;
			case OperationDefines.PROPERTYISGREATERTHANOREQUALTO :
				operationString = ">=  ";
				break;
			case OperationDefines.PROPERTYISLIKE :
				operationString = " is like ";
				break;
			case OperationDefines.PROPERTYISNULL :
				operationString = "is NULL ";
				break;
			case OperationDefines.PROPERTYISBETWEEN :
				operationString = " is between ";
				break;
		}

		Debug.debugMethodEnd();
		return operationString;
	}

	/**
     * sets the label of the <tt>LegendElement</tt>
     *
     * @param label label of the <tt>LegendElement</tt>
     */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * returns the label set to <tt>LegendElement</tt>. If no label is set, the
	 * method returns <tt>null</tt>
	 *
	 * @return label of the <tt>LegendElement</tt> or <tt>null</tt>
	 */
	public String getLabel() {
		return this.label;
	}

	/**
	 * @return
	 */
	protected String getLegendTitle() {
		return this.legendtitle;
	}

	/**
	 * @param string
	 */
	private void setLegendTitle(String string) {
		this.legendtitle = string;
	}

	/**
	 * @return
	 */
	private String getLegendTitleFilterProperty() {
		return legendtitlefilterproperty;
	}

	/**
	 * @param string
	 */
	private void setLegendTitleFilterProperty(String string) {
		legendtitlefilterproperty = string;
	}

}