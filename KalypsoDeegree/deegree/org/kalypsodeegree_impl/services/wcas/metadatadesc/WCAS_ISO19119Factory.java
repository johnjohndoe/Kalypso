/*----------------    FILE HEADER  ------------------------------------------
 
This file is part of Deegree.
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
 
Markus Mueller
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: mm@giub.uni-bonn.de
 
---------------------------------------------------------------------------*/

package org.deegree_impl.services.wcas.metadatadesc;

import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;

import org.deegree.services.wcas.metadatadesc.AccessProperties;
import org.deegree.services.wcas.metadatadesc.Address;
import org.deegree.services.wcas.metadatadesc.Citation;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;
import org.deegree.services.wcas.metadatadesc.ClassificationCode;
import org.deegree.services.wcas.metadatadesc.ConnectPoint;
import org.deegree.services.wcas.metadatadesc.ContactInfo;
import org.deegree.services.wcas.metadatadesc.DataCoupling;
import org.deegree.services.wcas.metadatadesc.DataType;
import org.deegree.services.wcas.metadatadesc.Date;
import org.deegree.services.wcas.metadatadesc.Dcp;
import org.deegree.services.wcas.metadatadesc.DependsOn;
import org.deegree.services.wcas.metadatadesc.EnumValues;
import org.deegree.services.wcas.metadatadesc.FunctionCode;
import org.deegree.services.wcas.metadatadesc.ISO19115;
import org.deegree.services.wcas.metadatadesc.ISO19119;
import org.deegree.services.wcas.metadatadesc.InstanceValue;
import org.deegree.services.wcas.metadatadesc.Keywords;
import org.deegree.services.wcas.metadatadesc.LegalConstraints;
import org.deegree.services.wcas.metadatadesc.Linkage;
import org.deegree.services.wcas.metadatadesc.MaximumValue;
import org.deegree.services.wcas.metadatadesc.MinimumValue;
import org.deegree.services.wcas.metadatadesc.OnLineResource;
import org.deegree.services.wcas.metadatadesc.OperationMetadata;
import org.deegree.services.wcas.metadatadesc.OperationName;
import org.deegree.services.wcas.metadatadesc.Parameter;
import org.deegree.services.wcas.metadatadesc.ParameterName;
import org.deegree.services.wcas.metadatadesc.ParameterType;
import org.deegree.services.wcas.metadatadesc.PermittedValues;
import org.deegree.services.wcas.metadatadesc.Phone;
import org.deegree.services.wcas.metadatadesc.PointOfContact;
import org.deegree.services.wcas.metadatadesc.PresentationFormCode;
import org.deegree.services.wcas.metadatadesc.PropertyRightsCode;
import org.deegree.services.wcas.metadatadesc.Quality;
import org.deegree.services.wcas.metadatadesc.Range;
import org.deegree.services.wcas.metadatadesc.ResourceSpecifiedUsage;
import org.deegree.services.wcas.metadatadesc.RoleCode;
import org.deegree.services.wcas.metadatadesc.SecurityConstraints;
import org.deegree.services.wcas.metadatadesc.ServiceType;
import org.deegree.services.wcas.metadatadesc.StatusCode;
import org.deegree.services.wcas.metadatadesc.TypeCode;
import org.deegree.services.wcas.metadatadesc.TypeName;
import org.deegree.services.wcas.metadatadesc.TypeProperty;
import org.deegree.services.wcas.metadatadesc.TypeValue;
import org.deegree.services.wcas.metadatadesc.UseConstraintsCode;
import org.deegree.services.wcas.metadatadesc.UserContactInfo;
import org.deegree.services.wcas.metadatadesc.Value;
import org.deegree.services.wcas.metadatadesc.ValueOnLineResource;
import org.deegree.xml.XMLTools;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * <p>WCAS_ISO19119Factory.java<br>
 * Created on 23. September 2002, 16:48</p>
 *
 * <p>Factory class for creating WCAS_Protocol classes from a XML document
 * that's conform to the WCAS-specification.</p>
 *
 * <p>----------------------------------------------------------------------</p>
 * @author <a href="mailto:schaefer@lat-lon.de>Axel Schaefer</a>
 * @version 2002-09-23
 */
public final class WCAS_ISO19119Factory {
    
    // NS = NameSpace
    static String NS119 = "www.gdi-nrw.org/iso19119Full";
    static String NS115FULL = "www.gdi-nrw.org/iso19115Full";
    static String NS19119GDIFull = "www.gdi-nrw.org/iso19119GDIFull";
    
    /**
     * factory method for creating a <tt>ISO19119</tt> object from
     * a file that contains a WCAS-specification conform XML capabilities document
     */
    public static synchronized ISO19119 createISO19119(Reader reader)
    throws Exception {
        
        Debug.level = Debug.ERRORS_AND_COMMENTS; // ERRORS_AND_COMMENTS
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "createISO19119(Reader)" );
        Document doc = XMLTools.parse( reader );
        ISO19119 iso19119 = createISO19119( doc );
        
        Debug.debugMethodEnd();
        
        return iso19119;
    }
    
    /**
     * factory method for creating a <tt>ISO19119</tt> object from
     * a WCTS-specification conform XML capabilities document. It calls the
     * methods, which creates instances of the child elements of the root.
     * if these aren't "complexTypes", strings or string-arrays are returned.
     */
    public static synchronized ISO19119 createISO19119(Document doc) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "createISO19119(Document)" );
        
        Element root = doc.getDocumentElement();
        
        // ########################### //
        // gets the different elements //
        // ########################### //
        
        // gets the seviceType section
        Element element = XMLTools.getNamedChild( root, NS119, "serviceType" );
        ServiceType servicetype = getServiceType( element );
        
        // gets the serviceTypeVersion
        element = XMLTools.getNamedChild( root, NS119, "serviceTypeVersion" );
        String servicetypeversion =  element.getFirstChild().getNodeValue();
        
        // citation
        element = XMLTools.getNamedChild( root, NS119, "citation" );
        Citation citation = getCitation( element );
        
        // abstract
        element = XMLTools.getNamedChild( root, NS119, "abstract");
        String abstract_ = element.getFirstChild().getNodeValue();
        
        // accessProperties
        element = XMLTools.getNamedChild( root, NS119, "accessProperties" );
        AccessProperties accessproperties = getAccessProperties( element );
        
        // keywords: minOccurs="0" maxOccurs="unbounded"
        NodeList nl = root.getElementsByTagNameNS(NS119, "keywords");
        ArrayList keywordslist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                keywordslist.add(i, getKeywords((Element)nl.item(i)));
            }
        }
        Keywords[] keywords = (Keywords[])keywordslist.toArray( new Keywords[keywordslist.size()] );
        
        // purpose: minOccurs="0"
        element = XMLTools.getNamedChild( root, NS119, "purpose" );
        String purpose = "";
        if (element != null) {
            purpose = element.getFirstChild().getNodeValue();
        }
        
        // credit: minOccurs="0"
        element = XMLTools.getNamedChild( root, NS119, "credit" );
        String credit = "";
        if (element != null) {
            credit = element.getFirstChild().getNodeValue();
        }
        
        // statusCode
        nl = root.getElementsByTagNameNS(NS119, "statusCode");
        ArrayList statuscodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                statuscodelist.add(i, getStatusCode((Element)nl.item(i)));
            }
        }
        StatusCode[]  statuscode= (StatusCode[])statuscodelist.toArray( new StatusCode[statuscodelist.size()] );
        
        // pointOfContact
        nl = root.getElementsByTagNameNS(NS119, "pointOfContact");
        ArrayList pointofcontactlist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                pointofcontactlist.add(i, getPointOfContact((Element)nl.item(i)));
            }
        }
        PointOfContact[] pointofcontact = (PointOfContact[])pointofcontactlist.toArray( new PointOfContact[pointofcontactlist.size()] );
        
        // resourceSpecifiedUsage
        nl = root.getElementsByTagNameNS(NS119, "resourceSpecifiedUsage");
        ArrayList resourcespecifiedusagelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                resourcespecifiedusagelist.add(i, getResourceSpecifiedUsage((Element)nl.item(i)));
            }
        }
        ResourceSpecifiedUsage[] resourcespecifiedusage = (ResourceSpecifiedUsage[])resourcespecifiedusagelist.toArray( new ResourceSpecifiedUsage[resourcespecifiedusagelist.size()] );
        
        // typeProperty
        nl = root.getElementsByTagNameNS(NS119, "typeProperty");
        ArrayList typepropertylist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                typepropertylist.add(i, getTypeProperty((Element)nl.item(i)));
            }
        }
        TypeProperty[] typeproperty = (TypeProperty[])typepropertylist.toArray( new TypeProperty[typepropertylist.size()] );
        
        // legalConstraints
        nl = root.getElementsByTagNameNS(NS119, "legalConstraints");
        ArrayList legalconstraintslist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                legalconstraintslist.add(i, getLegalConstraints((Element)nl.item(i)));
            }
        }
        LegalConstraints[] legalconstraints = (LegalConstraints[])legalconstraintslist.toArray( new LegalConstraints[legalconstraintslist.size()] );
        
        // securityConstraints
        nl = root.getElementsByTagNameNS(NS119, "securityConstraints");
        ArrayList securityconstraintslist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                securityconstraintslist.add(i, getSecurityConstraints((Element)nl.item(i)));
            }
        }
        SecurityConstraints[] securityconstraints = (SecurityConstraints[])securityconstraintslist.toArray( new SecurityConstraints[securityconstraintslist.size()] );
        
        // quality
        element = XMLTools.getNamedChild( root, NS119, "quality" );
        Quality quality = null;
        if (element != null) {
            quality = getQuality( element );
        }
        
        // operationMetadata
        nl = root.getElementsByTagNameNS(NS119, "operationMetadata");
        ArrayList operationmetadatalist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                operationmetadatalist.add(i, getOperationMetadata((Element)nl.item(i)));
            }
        }
        OperationMetadata[] operationmetadata = (OperationMetadata[])operationmetadatalist.toArray( new OperationMetadata[operationmetadatalist.size()] );

        
        // LatLonBoundingBox: minOccurs="0"
        element = XMLTools.getNamedChild( root, NS115FULL, "LatLonBoundingBox" );
        String latlonboundingbox = "";
        if (element != null) {
            latlonboundingbox = element.getFirstChild().getNodeValue();
        }
        
        // dataCoupling
        element = XMLTools.getNamedChild( root, NS119, "dataCoupling" );
        DataCoupling datacoupling = getDataCoupling( element );
        
        // MD_Metadata (ISO19115)
        element = XMLTools.getNamedChild( root, NS115FULL, "MD_Metadata" );
        ISO19115[] md_metadata = getMD_Metadata( element );
        
//        System.out.println("\n\n\n---===---");
//        
//        System.out.println("abstract: " + abstract_);
//        System.out.println("accessproperties: " + accessproperties);
//        System.out.println("citation: " + citation);
//        System.out.println("credit: " + credit);
//        System.out.println("datacoupling: " + datacoupling);
//        System.out.println("keywords: " + keywords);
//        System.out.println("md_metadata: " + md_metadata);
//        System.out.println("operationmetadata: " + operationmetadata);
//        System.out.println("pointofcontact: " + pointofcontact);
//        System.out.println("purpose: " + purpose);
//        System.out.println("quality: " + quality);
//        System.out.println( "resourcespecifiedusage: " +  resourcespecifiedusage);
//        System.out.println("securityconstraints: " + securityconstraints);
//        System.out.println("servicetype: " + servicetype);
//        System.out.println("servicetypeversion: " + servicetypeversion);
//        System.out.println("statuscode: " + statuscode);
//        System.out.println("typeproperty: " + typeproperty);
        
        ISO19119 iso19119 = new ISO19119_Impl(abstract_,
        accessproperties,
        citation,
        credit,
        datacoupling,
        keywords,
        md_metadata,
        operationmetadata,
        pointofcontact,
        purpose,
        quality,
        resourcespecifiedusage,
        securityconstraints,
        servicetype,
        servicetypeversion,
        statuscode,
        typeproperty);
       
        Debug.debugMethodEnd();
        return iso19119;
    }
    
    
    /**
     * returns an instance of the element &lt;serviceType&gt;<br>
     */
    private static ServiceType getServiceType(Element serviceTypeElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getServiceType" );
        
        // element nameValue
        Element element = XMLTools.getNamedChild(serviceTypeElement, NS119, "nameValue");
        String namevalue = element.getFirstChild().getNodeValue();
        
        // element nameNameSpace
        element = XMLTools.getNamedChild(serviceTypeElement, NS119, "nameNameSpace");
        String namenamespace = element.getFirstChild().getNodeValue();
        
        ServiceType st = new ServiceType_Impl(namenamespace, namevalue);
        
        Debug.debugMethodEnd();
        return st;
    }
    
    /**
     * returns an instance of the element &lt;citation&gt;
     */
    private static Citation getCitation(Element citationElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getCitation" );
        
        // title
        Element element = XMLTools.getNamedChild(citationElement, NS119, "title");
        String title = element.getFirstChild().getNodeValue();
        
        // alternateTitle minOccurs="0" maxOccurs="unbounded"
        NodeList nl = citationElement.getElementsByTagNameNS( NS119, "alternateTitle" );
        ArrayList alternatetitlelist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                alternatetitlelist.add(i, nl.item(i).getFirstChild().getNodeValue());
            }
        }
        String[] alternatetitle = (String[])alternatetitlelist.toArray( new String[alternatetitlelist.size()] );
        
        // date
        nl = citationElement.getElementsByTagNameNS( NS119, "date" );
        Date[] date = new Date[ nl.getLength() ];
        for(int i=0; i<nl.getLength(); i++) {
            date[i] = getDate( (Element)nl.item(i) );
        }
        
        // edition: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "edition");
        String edition = "";
        if (element != null) {
            edition = element.getFirstChild().getNodeValue();
        }
        
        // editionDate: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "editionDate");
        String editiondate = "";
        if (element != null) {
            editiondate = element.getFirstChild().getNodeValue();
        }
        
        // identifier
        nl = citationElement.getElementsByTagNameNS( NS119, "identifier" );
        ArrayList identifierlist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                // nl.item(i).getFirstChild().getNodeValue();
                identifierlist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] identifier = (String[])identifierlist.toArray( new String[identifierlist.size()] );
        
        // identifierType
        nl = citationElement.getElementsByTagNameNS( NS119, "identifierType" );
        ArrayList identifiertypelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                identifiertypelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] identifiertype = (String[])identifiertypelist.toArray( new String[identifiertypelist.size()] );
        
        // citedResponsibleParty
        nl = citationElement.getElementsByTagNameNS( NS119, "citedResponsibleParty" );
        CitedResponsibleParty[] citedresponsibleparty = new CitedResponsibleParty[ nl.getLength() ];
        if (nl != null && nl.getLength() > 0) {
            for(int i=0; i < nl.getLength(); i++) {
                citedresponsibleparty[i] = getCitedResponsibleParty( (Element)nl.item(i) );
            }
        }
        
        // presentationFormCode
        nl = citationElement.getElementsByTagNameNS(  NS119, "presentationFormCode" );
        ArrayList presentationformcodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for(int i=0; i < length; i++) {
                presentationformcodelist.add(i, getPresentationFormCode( (Element)nl.item(i) ));
            }
        }
        PresentationFormCode[] presentationformcode = (PresentationFormCode[])presentationformcodelist.toArray( new PresentationFormCode[presentationformcodelist.size()] );
        
        // seriesName: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "seriesName");
        String seriesname = "";
        if (element != null) {
            seriesname = element.getFirstChild().getNodeValue();
        }
        
        // issueIdentification: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "issueIdentification");
        String issueidentification = "";
        if (element != null) {
            issueidentification = element.getFirstChild().getNodeValue();
        }
        
        // otherCitationDetails: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "otherCitationDetails");
        String othercitationdtails = "";
        if (element != null) {
            othercitationdtails = element.getFirstChild().getNodeValue();
        }
        
        // collectionTitle: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "collectionTitle");
        String collectiontitle = "";
        if (element != null) {
            collectiontitle = element.getFirstChild().getNodeValue();
        }
        
        // page: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "page");
        String page = "";
        if (element != null) {
            page = element.getFirstChild().getNodeValue();
        }
        
        // ISBN: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "ISBN");
        String isbn = "";
        if (element != null) {
            isbn = element.getFirstChild().getNodeValue();
        }
        
        // ISSN: minOccurs="0"
        element = XMLTools.getNamedChild(citationElement, NS119, "ISSN");
        String issn = "";
        if (element != null) {
            issn = element.getFirstChild().getNodeValue();
        }
        
        Citation citation = new Citation_Impl( alternatetitle,
        citedresponsibleparty,
        collectiontitle,
        date,
        edition,
        editiondate,
        identifier,
        identifiertype,
        isbn,
        issn,
        issueidentification,
        othercitationdtails,
        page,
        presentationformcode,
        seriesname,
        title );
        Debug.debugMethodEnd();
        return citation;
    }
    
    
    /**
     * returns an instance of the element &lt;Date&gt;<br>
     * <tt> use="required", value="creation" or "publication" or "revision"<br>
     * maxOccurs="unbounded"</tt>
     */
    private static Date getDate(Element dateElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getDate");
        Date date = null;
        String datetype = XMLTools.getAttrValue(dateElement, "dateType");
        if ( datetype.equals("creation") ||
        datetype.equals("publication") ||
        datetype.equals("revision")) {
            date = new Date_Impl(datetype);
        } else {
            throw new Exception( "wrong date format! " +  dateElement);
        }
        Debug.debugMethodEnd();
        return date;
    }
    
    /**
     * returns an instance of the element &lt;citedResponsibleParty&gt;
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static CitedResponsibleParty getCitedResponsibleParty(Element crpElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getCitedResponsibleParty");
        
        // individualName minOccurs="0" maxOccurs="unbounded"
        NodeList nl = crpElement.getElementsByTagNameNS( NS119, "individualName" );
        ArrayList individualnamelist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                individualnamelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] individualname = (String[])individualnamelist.toArray( new String[individualnamelist.size()] );
        
        // organisationName minOccurs="0" maxOccurs="unbounded"
        nl = crpElement.getElementsByTagNameNS( NS119, "organisationName" );
        ArrayList organisationnamelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                organisationnamelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] organisationname = (String[])organisationnamelist.toArray( new String[organisationnamelist.size()] );
        
        
        // positionName minOccurs="0" maxOccurs="unbounded"
        nl = crpElement.getElementsByTagNameNS( NS119, "positionName" );
        ArrayList positionnamelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                positionnamelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] positionname = (String[])positionnamelist.toArray( new String[positionnamelist.size()] );
        
        // contactInfo
        nl = crpElement.getElementsByTagNameNS( NS119, "contactInfo" );
        ArrayList contactinfolist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                contactinfolist.add(i, getContactInfo( (Element)nl.item(i) ));
            }
        }
        ContactInfo[] contactinfo = (ContactInfo[])contactinfolist.toArray( new ContactInfo[contactinfolist.size()] );
        
        // roleCode maxOccurs="unbounded"
        nl = crpElement.getElementsByTagNameNS( NS119, "roleCode" );
        ArrayList rolecodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for(int i=0; i < length; i++) {
                rolecodelist.add(i, getRoleCode( (Element)nl.item(i) ));
            }
        }
        RoleCode[] rolecode = (RoleCode[])rolecodelist.toArray( new RoleCode[rolecodelist.size()] );
        
        CitedResponsibleParty crp = new CitedResponsibleParty_Impl(
        contactinfo, individualname, organisationname, positionname,
        rolecode);
        
        Debug.debugMethodEnd();
        return crp;
    }
    
    /**
     * returns an instance of the element &lt;contactInfo&gt;
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static ContactInfo getContactInfo(Element contactInfoElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getContactInfo");
        
        // phone minOccurs="0"
        Element element = XMLTools.getNamedChild(contactInfoElement, NS119, "phone");
        Phone phone = null;
        if (element != null) {
            phone = getPhone(element);
        }
        
        // address minOccurs="0"
        element = XMLTools.getNamedChild(contactInfoElement, NS119, "address");
        Address address = null;
        if (element != null) {
            address = getAddress(element);
        }
        
        // onLineResource minOccurs="0"
        element = XMLTools.getNamedChild(contactInfoElement, NS119, "onLineResource");
        OnLineResource onlineresource = null;
        if (element != null) {
            onlineresource = getOnLineResource(element);
        }
        
        // hoursOfService minOccurs="0"
        element = XMLTools.getNamedChild(contactInfoElement, NS119, "hoursOfService");
        String hoursofservice = "";
        if (element != null) {
            hoursofservice = element.getFirstChild().getNodeValue();
        }
        
        // contactInstructions minOccurs="0"
        element = XMLTools.getNamedChild(contactInfoElement, NS119, "contactInstructions");
        String contactinstructions = "";
        if (element != null) {
            contactinstructions = element.getFirstChild().getNodeValue();
        }
        
        ContactInfo ci = new ContactInfo_Impl( address, contactinstructions,
        hoursofservice, onlineresource, phone );
        
        Debug.debugMethodEnd();
        return ci;
    }
    
    /**
     * returns an instance of the element &lt;phone&gt;
     * <tt>minOccurs="0"</tt>
     */
    private static Phone getPhone(Element phoneElement) {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getPhone");
        
        // facsimile minOccurs="0" maxOccurs="unbounded"
        NodeList nl = phoneElement.getElementsByTagNameNS( NS119, "facsimile" );
        ArrayList facsimilelist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                facsimilelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] facsimile = (String[])facsimilelist.toArray( new String[facsimilelist.size()] );
        
        // other minOccurs="0" maxOccurs="unbounded"
        nl = phoneElement.getElementsByTagNameNS( NS119, "other" );
        ArrayList otherlist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                otherlist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] other = (String[])otherlist.toArray( new String[otherlist.size()] );
        
        // otherType minOccurs="0" maxOccurs="unbounded"
        nl = phoneElement.getElementsByTagNameNS( NS119, "otherType" );
        ArrayList othertypelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                othertypelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] othertype = (String[])othertypelist.toArray( new String[othertypelist.size()] );
        
        // voice minOccurs="0" maxOccurs="unbounded"
        nl = phoneElement.getElementsByTagNameNS( NS119, "voice" );
        ArrayList voicelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                voicelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] voice = (String[])voicelist.toArray( new String[voicelist.size()] );
        
        Phone phone = new Phone_Impl( facsimile, other, othertype, voice );
        Debug.debugMethodEnd();
        return phone;
    }
    
    /**
     * returns an instance of the element &lt;address&gt;
     * <tt>minOccurs="0"</tt>
     */
    private static Address getAddress(Element addressElement) {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getAddress");
        
        // deliveryPoint minOccurs="0" maxOccurs="unbounded"
        NodeList nl = addressElement.getElementsByTagNameNS( NS119, "deliveryPoint" );
        ArrayList deliverypointlist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                deliverypointlist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] deliverypoint = (String[])deliverypointlist.toArray( new String[deliverypointlist.size()] );
        
        // city minOccurs="0"
        Element element = XMLTools.getNamedChild(addressElement, NS119, "city");
        String city = "";
        if(element != null) {
            city = element.getFirstChild().getNodeValue();
        }
        
        // administrativeArea minOccurs="0"
        element = XMLTools.getNamedChild(addressElement, NS119, "administrativeArea");
        String administrativearea = "";
        if(element != null) {
            administrativearea = element.getFirstChild().getNodeValue();
        }
        
        // postalCode minOccurs="0"
        element = XMLTools.getNamedChild(addressElement, NS119, "postalCode");
        String postalcode = "";
        if(element != null) {
            postalcode = element.getFirstChild().getNodeValue();
        }
        
        // country minOccurs="0"
        element = XMLTools.getNamedChild(addressElement, NS119, "country");
        String country = "";
        if(element != null) {
            country = element.getFirstChild().getNodeValue();
        }
        
        // electronicMailAddress minOccurs="0" maxOccurs="unbounded"
        nl = addressElement.getElementsByTagNameNS( NS119, "electronicMailAddress" );
        ArrayList electronicmailaddresslist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                electronicmailaddresslist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] electronicmailaddress = (String[])electronicmailaddresslist.toArray( new String[electronicmailaddresslist.size()] );
        
        Address address = new Address_Impl( administrativearea,
        city,
        country,
        deliverypoint,
        electronicmailaddress,
        postalcode );
        
        Debug.debugMethodEnd();
        return address;
    }
    
    /**
     * returns an instance of the element &lt;onLineResource&gt;
     * <tt>minOccurs="0"</tt>
     */
    private static OnLineResource getOnLineResource(Element olrElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getOnLineResource");
        
        // linkage
        Element element = XMLTools.getNamedChild(olrElement, NS119, "linkage");
        Linkage linkage = getLinkage(element);
        
        // protocol minOccurs="0"
        element = XMLTools.getNamedChild(olrElement, NS119, "protocol");
        String protocol = "";
        if(element != null) {
            protocol = element.getFirstChild().getNodeValue();
        }
        
        // applicationProfile minOccurs="0"
        element = XMLTools.getNamedChild(olrElement, NS119, "applicationProfile");
        String applicationprofile = "";
        if (element != null) {
            applicationprofile = element.getFirstChild().getNodeValue();
        }
        
        // onlineResourceName minOccurs="0"
        element = XMLTools.getNamedChild(olrElement, NS119, "onlineResourceName");
        String onlineresourcename = "";
        if (element != null) {
            onlineresourcename = element.getFirstChild().getNodeValue();
        }
        
        // onlineResourceDescription minOccurs="0"
        element = XMLTools.getNamedChild(olrElement, NS119, "onlineResourceDescription");
        String onlineresourcedescription = "";
        if (element != null) {
            onlineresourcedescription = element.getFirstChild().getNodeValue();
        }
        
        // functionCode minOccurs="0"
        element = XMLTools.getNamedChild(olrElement, NS119, "functionCode");
        FunctionCode functioncode = null;
        if (element != null) {
            functioncode = getFunctionCode(element);
        }
        
        OnLineResource onlineresource = new OnLineResource_Impl( applicationprofile,
        functioncode,
        linkage,
        onlineresourcedescription,
        onlineresourcename,
        protocol );
        Debug.debugMethodEnd();
        return onlineresource;
    }
    
    /**
     * returns an instance of the element &lt;linkage&gt;
     */
    private static Linkage getLinkage(Element linkageElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getLinkage");
        
        // xlink fixed="http://www.w3.org/1999/xlink"
        String xlink = "http://www.w3.org/1999/xlink";
        URL xlinkurl = new URL(xlink);
        if (!(linkageElement.getAttribute("xlink").equals(xlink))) {
            throw new Exception("wrong xlink argument! " + linkageElement);
        }
        
        // type fixed="simple"
        String type = "simple";
        if (!(linkageElement.getAttribute("type").equals(type))) {
            throw new Exception("wrong type argument! " + linkageElement);
        }
        
        // href use="required"
        URL hrefurl = null;
        String href = "";
        if ( linkageElement.getAttribute("href").length() > 0 ) {
            href = linkageElement.getAttribute("href");
            hrefurl = new URL(href);
        } else {
            throw new Exception("href attribute required! " + linkageElement);
        }
        
        Linkage linkage = new Linkage_Impl( hrefurl, type, xlinkurl );
        Debug.debugMethodEnd();
        return linkage;
    }
    
    /**
     * returns an instance of the element &lt;functionCode&gt;<br>
     * <tt>minOccurs="0"</tt><br>
     * contains the attribute value. use=required. possible values are
     * access, additionalInformation, download, order, search
     */
    private static FunctionCode getFunctionCode(Element fcElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getFunctionCode");
        
        String functioncode = fcElement.getAttribute("value");
        
        if ( functioncode.length() > 0 ) {
            // negotiation of the required values
            if ( !( (functioncode.equals("access")) ||
            (functioncode.equals("additionalInformation")) ||
            (functioncode.equals("download")) ||
            (functioncode.equals("order")) ||
            (functioncode.equals("search")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + fcElement);
            }
        } else throw new Exception("value attribute required! " + fcElement);
        
        FunctionCode fCode = new FunctionCode_Impl( functioncode );
        Debug.debugMethodEnd();
        return fCode;
    }
    
    
    /**
     * returns an instance of the element &lt;roleCode&gt;<br>
     * <tt>maxOccurs="unbounded"</tt><br>
     * <pre> &lt;xs:complexType name="roleCodeType"&gt;
     * &lt;xs:attribute name="value" use="required"&gt;
     * &lt;xs:simpleType&gt;
     * &lt;xs:restriction base="xs:NMTOKEN"&gt;
     * &lt;xs:enumeration value="contentProvider"/&gt;
     * &lt;xs:enumeration value="custodianSteward"/&gt;
     * &lt;xs:enumeration value="owner"/&gt;
     * &lt;xs:enumeration value="user"/&gt;
     * &lt;xs:enumeration value="distributor"/&gt;
     * &lt;xs:enumeration value="metadataProvider"/&gt;
     * &lt;xs:enumeration value="originator"/&gt;
     * &lt;xs:enumeration value="pointOfContact"/&gt;
     * &lt;xs:enumeration value="principalInvestigator"/&gt;
     * &lt;xs:enumeration value="processor"/&gt;
     * &lt;xs:enumeration value="publisher"/&gt;
     * &lt;/xs:restriction&gt;
     * &lt;/xs:simpleType&gt;
     * &lt;/xs:attribute&gt;
     * &lt;/xs:complexType&gt;
     * </pre>
     */
    private static RoleCode getRoleCode(Element rcElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getRoleCode");
        
        String value = rcElement.getAttribute("value");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("contentProvider")) ||
            (value.equals("custodianSteward")) ||
            (value.equals("owner")) ||
            (value.equals("user")) ||
            (value.equals("distributor")) ||
            (value.equals("metadataProvider")) ||
            (value.equals("originator")) ||
            (value.equals("pointOfContact")) ||
            (value.equals("principalInvestigator")) ||
            (value.equals("processor")) ||
            (value.equals("publisher")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + rcElement);
            }
        } else throw new Exception("value attribute required! " + rcElement);
        
        RoleCode roco = new RoleCode_Impl( value );
        Debug.debugMethodEnd();
        return roco;
    }
    
    
    
    // #########################################################################
    
    
    /**
     * returns an instance of the element &lt;presentationFormCode&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt><br>
     * possible values of the value-attribute are: document, hardcopyMap, image,
     * model, profile, rasterMap, table, vectorMap, view
     */
    private static PresentationFormCode getPresentationFormCode(Element pfcElement)
    throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getPresentationFormCode");
        
        String value = pfcElement.getAttribute("value");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("document")) ||
            (value.equals("hardcopyMap")) ||
            (value.equals("image")) ||
            (value.equals("model")) ||
            (value.equals("profile")) ||
            (value.equals("rasterMap")) ||
            (value.equals("table")) ||
            (value.equals("vectorMap")) ||
            (value.equals("view")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + pfcElement);
            }
        } else throw new Exception("value attribute required! " + pfcElement);
        
        PresentationFormCode prefoco = new PresentationFormCode_Impl( value );
        
        Debug.debugMethodEnd();
        return prefoco;
    }
    
    // #########################################################################
    
    /**
     * returns an instance of the element &lt;accessProperties&gt;
     */
    private static AccessProperties getAccessProperties(Element apElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getAccessProperties" );
        
        // fees minOccurs="0" (String)
        Element element = XMLTools.getNamedChild(apElement, NS119, "fees");
        String fees = "";
        if (element != null) {
            fees = element.getFirstChild().getNodeValue();
        }
        
        // plannedAvailableDateTime" minOccurs="0" (String)
        element = XMLTools.getNamedChild(apElement, NS119, "plannedAvailableDateTime");
        String plannedavailabledatetime = "";
        if (element != null) {
            plannedavailabledatetime = element.getFirstChild().getNodeValue();
        }
        
        // orderingInstructions" minOccurs="0" (String)
        element = XMLTools.getNamedChild(apElement, NS119, "orderingInstructions");
        String orderinginstructions = "";
        if (element != null) {
            orderinginstructions = element.getFirstChild().getNodeValue();
        }
        
        // turnaround" minOccurs="0" (String)
        element = XMLTools.getNamedChild(apElement, NS119, "turnaround");
        String turnaround = "";
        if (element != null) {
            turnaround = element.getFirstChild().getNodeValue();
        }
        
        AccessProperties accessProperties = new AccessProperties_Impl( fees, orderinginstructions, plannedavailabledatetime, turnaround );
        Debug.debugMethodEnd();
        return accessProperties;
    }
    
    /**
     * returns the keywords associated with the service<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt><br>
     * <pre>
     * 	&lt;xs:complexType name="keywordsType"&gt;
     * &lt;xs:sequence&gt;
     * &lt;xs:element ref="iso19119:keyword" minOccurs="0" maxOccurs="unbounded"/&gt;
     * &lt;xs:element name="typeCode" type="iso19119:typeCodeType" minOccurs="0"/&gt;
     * &lt;xs:element ref="iso19119:thesaurusName" minOccurs="0"/&gt;
     * &lt;/xs:sequence&gt;
     * &lt;/xs:complexType&gt;
     * </pre>
     */
    private static Keywords getKeywords(Element keywordsElement)
    throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getKeywords" );
        
        // keyword minOccurs="0" maxOccurs="unbounded"
        NodeList one_kw = keywordsElement.getElementsByTagNameNS( NS119, "keyword" );
        String[] kw = new String[ one_kw.getLength() ];
        for (int i = 0; i < kw.length; i++) {
            kw[i] = one_kw.item(i).getFirstChild().getNodeValue();
        }
        
        // typeCode
        Element element = XMLTools.getNamedChild( keywordsElement, NS119, "typeCode");
        TypeCode typecode = null;
        if (element != null) {
            typecode = getTypeCode(element);
        }
        
        // thesaurusName minOccurs="0"
        element = XMLTools.getNamedChild( keywordsElement, NS119, "thesaurusName");
        String thesaurusname = "";
        if (element != null) {
            thesaurusname = element.getFirstChild().getNodeValue();
        }
        
        Keywords keywords = new Keywords_Impl(kw, thesaurusname, typecode);
        Debug.debugMethodEnd();
        return keywords;
    }
    
    /**
     * returns an instance of the element &lt;statusCode&gt;<br>
     * minOccurs="0"
     * &lt;xs:attribute name="KeyType" use="required"&gt;
     * values: discipline, place, stratum, temporal, theme
     */
    private static TypeCode getTypeCode(Element tcElement)
    throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getTypeCode");
        
        String value = tcElement.getAttribute("KeyType");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("discipline")) ||
            (value.equals("place")) ||
            (value.equals("stratum")) ||
            (value.equals("temporal")) ||
            (value.equals("theme")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + tcElement);
            }
        } else throw new Exception("value attribute required! " + tcElement);
        
        TypeCode typecode = new TypeCode_Impl( value );
        
        Debug.debugMethodEnd();
        return typecode;
    }
    
    /**
     * returns an instance of the element &lt;statusCode&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt><br>
     * &lt;xs:attribute name="progressCode" use="required"&gt;
     * values: completed, historicalArchive, obsolete, onGoing, planned,
     * required, underdevelopment
     */
    private static StatusCode getStatusCode(Element scElement)
    throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getStatusCode" );
        
        String value = scElement.getAttribute("progressCode");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("completed")) ||
            (value.equals("historicalArchive")) ||
            (value.equals("obsolete")) ||
            (value.equals("onGoing")) ||
            (value.equals("planned")) ||
            (value.equals("required")) ||
            (value.equals("underdevelopment")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + scElement);
            }
        } else throw new Exception("value attribute required! " + scElement);
        
        StatusCode statuscode = new StatusCode_Impl( value );
        
        Debug.debugMethodEnd();
        return statuscode;
    }
    
    /**
     * returns the PointOfContact-element.<br>
     * <tt>&lt;xs:element name="pointOfContact" type="iso19119:pointOfContactType"
     * minOccurs="0" maxOccurs="unbounded"/&gt;</tt>
     */
    private static PointOfContact getPointOfContact(Element pocElement)
    throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getPointOfContact" );
        
        // individualName minOccurs="0" maxOccurs="unbounded" (String)
        NodeList nl = pocElement.getElementsByTagNameNS( NS119, "individualName" );
        String[] individualname = new String[ nl.getLength() ];
        for (int i = 0; i < individualname.length; i++) {
            individualname[i] = nl.item(i).getFirstChild().getNodeValue();
        }
        
        // organisationName minOccurs="0" maxOccurs="unbounded" (String)
        nl = pocElement.getElementsByTagNameNS( NS119, "organisationName" );
        String[] organisationname = new String[ nl.getLength() ];
        for (int i = 0; i < organisationname.length; i++) {
            organisationname[i] = nl.item(i).getFirstChild().getNodeValue();
        }
        
        // positionName minOccurs="0" maxOccurs="unbounded" (String)
        nl = pocElement.getElementsByTagNameNS( NS119, "positionName" );
        String[] positionname = new String[ nl.getLength() ];
        for (int i = 0; i < positionname.length; i++) {
            positionname[i] = nl.item(i).getFirstChild().getNodeValue();
        }
        
        // contactInfo minOccurs="0" maxOccurs="unbounded"
        nl = pocElement.getElementsByTagNameNS( NS119, "contactInfo" );
        ArrayList contactinfolist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                contactinfolist.add(i, getContactInfo( (Element)nl.item(i) ));
            }
        }
        ContactInfo[] contactinfo = (ContactInfo[])contactinfolist.toArray( new ContactInfo[contactinfolist.size()] );
        
        // roleCode maxOccurs="unbounded"
        nl = pocElement.getElementsByTagNameNS( NS119, "roleCode" );
        ArrayList rolecodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for(int i=0; i < length; i++) {
                rolecodelist.add(i, getRoleCode( (Element)nl.item(i) ));
            }
        }
        RoleCode[] rolecode = (RoleCode[])rolecodelist.toArray( new RoleCode[rolecodelist.size()] );
        
        PointOfContact pointofcontact = new PointOfContact_Impl( contactinfo,
        individualname, organisationname, positionname, rolecode );
        
        Debug.debugMethodEnd();
        return pointofcontact;
    }
    
    /**
     * returns ResourceSpecifiedUsage<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static ResourceSpecifiedUsage getResourceSpecifiedUsage(Element rsuElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getResourceSpecifiedUsage" );
        
        // iso19119:specifiedUsage
        Element element = XMLTools.getNamedChild(rsuElement, NS119, "specifiedUsage");
        String specifiedusage = element.getFirstChild().getNodeValue();
        
        // iso19119:usageDateTime minOccurs="0"
        element = XMLTools.getNamedChild(rsuElement, NS119, "usageDateTime");
        String usagedatetime = "";
        if (element != null) {
            usagedatetime = element.getFirstChild().getNodeValue();
        }
        
        // iso19119:userDetirminedLimitations minOccurs="0"
        element = XMLTools.getNamedChild(rsuElement, NS119, "userDetirminedLimitations");
        String userdetirminedlimitations = "";
        if (element != null) {
            userdetirminedlimitations = element.getFirstChild().getNodeValue();
        }
        
        // userContactInfo maxOccurs="unbounded"/>
        NodeList nl = rsuElement.getElementsByTagNameNS(NS119, "userContactInfo");
        ArrayList usercontactinfolist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                usercontactinfolist.add(i, getUserContactInfo((Element)nl.item(i)));
            }
        }
        UserContactInfo[] usercontactinfo = (UserContactInfo[])usercontactinfolist.toArray( new UserContactInfo[usercontactinfolist.size()] );
        
        ResourceSpecifiedUsage resourcespecifiedusage =
        new ResourceSpecifiedUsage_Impl(
        specifiedusage,
        usagedatetime,
        usercontactinfo,
        userdetirminedlimitations );

        Debug.debugMethodEnd();
        return resourcespecifiedusage;
    }
    
    /**
     * returns an instance of the element &lt;userContactInfo&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static UserContactInfo getUserContactInfo(Element uciElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getUserContactInfo");
        
        // individualName minOccurs="0" maxOccurs="unbounded"
        NodeList nl = uciElement.getElementsByTagNameNS( NS119, "individualName" );
        ArrayList individualnamelist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                individualnamelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] individualname = (String[])individualnamelist.toArray( new String[individualnamelist.size()] );
        
        // organisationName minOccurs="0" maxOccurs="unbounded"
        nl = uciElement.getElementsByTagNameNS( NS119, "organisationName" );
        ArrayList organisationnamelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                organisationnamelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] organisationname = (String[])organisationnamelist.toArray( new String[organisationnamelist.size()] );
        
        
        // positionName minOccurs="0" maxOccurs="unbounded"
        nl = uciElement.getElementsByTagNameNS( NS119, "positionName" );
        ArrayList positionnamelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                positionnamelist.add(i, XMLTools.getValue(nl.item(i)));
            }
        }
        String[] positionname = (String[])positionnamelist.toArray( new String[positionnamelist.size()] );
        
        // contactInfo minOccurs="0" maxOccurs="unbounded"
        nl = uciElement.getElementsByTagNameNS( NS119, "contactInfo" );
        ArrayList contactinfolist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                contactinfolist.add(i, getContactInfo( (Element)nl.item(i) ));
            }
        }
        ContactInfo[] contactinfo = (ContactInfo[])contactinfolist.toArray( new ContactInfo[contactinfolist.size()] );
        
        // roleCode maxOccurs="unbounded"
        nl = uciElement.getElementsByTagNameNS( NS119, "roleCode" );
        ArrayList rolecodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for(int i=0; i < length; i++) {
                rolecodelist.add(i, getRoleCode( (Element)nl.item(i) ));
            }
        }
        RoleCode[] rolecode = (RoleCode[])rolecodelist.toArray( new RoleCode[rolecodelist.size()] );
        
        UserContactInfo uci = new UserContactInfo_Impl( contactinfo, individualname, organisationname, positionname, rolecode );
        
        Debug.debugMethodEnd();
        return uci;
    }
    
    /**
     * returns an instance of the element &lt;typeProperty&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt><br>
     * element name="typeName"
     * element name="typeValue"
     */
    private static TypeProperty getTypeProperty(Element tpElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getTypeProperty" );
        
        // typeName
        Element element = XMLTools.getNamedChild(tpElement, NS119, "typeName");
        TypeName typename = getTypeName(element);
        
        // typeValue
        element = XMLTools.getNamedChild(tpElement, NS119, "typeValue");
        TypeValue typevalue = getTypeValue(element);
        
        TypeProperty typeproperty = new TypeProperty_Impl(typename, typevalue);
        Debug.debugMethodEnd();
        return typeproperty;
    }
    
    
    /**
     * returns an instance of the element &lt;typeName&gt;<br>
     * &lt;xs:element ref="iso19119:nameValue"/&gt;
     * &lt;xs:element ref="iso19119:nameNameSpace"/&gt;
     */
    private static TypeName getTypeName(Element tnElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getTypeName" );
        
        // nameValue
        Element element = XMLTools.getNamedChild( tnElement, NS119, "nameValue" );
        String namevalue = element.getFirstChild().getNodeValue();
        
        // nameNameSpace
        element = XMLTools.getNamedChild( tnElement, NS119, "nameNameSpace" );
        String namenamespace= element.getFirstChild().getNodeValue();
        
        TypeName typename = null;//new TypeName_Impl( namenamespace, namevalue );
        
        Debug.debugMethodEnd();
        return typename;
    }
    
    /**
     * returns an instance of the element &lt;typeValue&gt;<br>
     * <b>Choice:</b><br>
     * &lt;xs:element name="dataType"/&gt;
     * &lt;xs:element name="instanceValue"/&gt;
     * &lt;xs:element name="range"/&gt;
     * &lt;xs:element name="enumValues"/&gt;
     */
    private static TypeValue getTypeValue(Element tvElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getTypeValue" );
        
        String tagwithns = XMLTools.getFirstElement(tvElement).getTagName();
        String tagname = XMLTools.toLocalName(tagwithns);
        
        if (tagname.equals("dataType")) {
            Element element = XMLTools.getNamedChild( tvElement, NS119, "dataType" );
            DataType datatype = getDataType(element);
        } else if (tagname.equals("instanceValue")) {
            Element element = XMLTools.getNamedChild( tvElement, NS119, "instanceValue" );
            InstanceValue instancevalue = getInstanceValue(element);
        }
        else if (tagname.equals("range")) {
            Element element = XMLTools.getNamedChild( tvElement, NS119, "range" );
            Range range = getRange(element);
        }
        else if (tagname.equals("enumValues")) {
            Element element = XMLTools.getNamedChild( tvElement, NS119, "enumValues" );
            EnumValues enumvalues = getEnumValues(element);
        }
        
        TypeValue typevalue = null;
        
        Debug.debugMethodEnd();
        return typevalue;
    }
    
    /**
     * returns an instance of the element &lt;dataType&gt;<br>
     * <pre>
     * &lt;xs:complexType name="dataTypeType"&gt;
     * &lt;xs:attribute name="type" use="required"&gt;
     * &lt;xs:simpleType&gt;
     * &lt;xs:restriction base="xs:NMTOKEN"&gt;
     * &lt;xs:enumeration value="string"/&gt;
     * &lt;xs:enumeration value="number"/&gt;
     * &lt;/xs:restriction&gt;
     * &lt;/xs:simpleType&gt;
     * &lt;/xs:attribute&gt;
     * &lt;/xs:complexType&gt;
     * </pre>
     */
    private static DataType getDataType(Element dtElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getDataType" );
        
        String value = dtElement.getAttribute("type");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("string")) ||
            (value.equals("number")) ) ) {
                throw new Exception("wrong value of the type-attribute: " + dtElement);
            }
        } else throw new Exception("type attribute required! " + dtElement);
        
        DataType datatype = new DataType_Impl( value );
        
        Debug.debugMethodEnd();
        return datatype;
    }
    
    /**
     * returns an instance of the element &lt;instanceValue&gt;<br>
     */
    private static InstanceValue getInstanceValue(Element ivElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getInstanceValue" );
        
        // iso19119:valueTitle minOccurs="0"
        Element element = XMLTools.getNamedChild( ivElement, NS119, "valueTitle");
        String valuetitle = "";
        if (element != null) {
            valuetitle = element.getFirstChild().getNodeValue();
        }
        
        // iso19119:valueDescription minOccurs="0"
        element = XMLTools.getNamedChild( ivElement, NS119, "valueDescription");
        String valuedescription = "";
        if (element != null) {
            valuedescription = element.getFirstChild().getNodeValue();
        }
        
        // valueOnLineResource minOccurs="0"
        element = XMLTools.getNamedChild( ivElement, NS119, "valueOnLineResource");
        ValueOnLineResource valueonlineresource = null;
        if (element != null) {
            valueonlineresource = getValueOnLineResource(element);
        }
        
        // value
        element = XMLTools.getNamedChild( ivElement, NS119, "value");
        Value value = null;
        if (element != null) {
            value = getValue(element);
        }
        
        InstanceValue instancevalue = new InstanceValue_Impl( value, valuedescription, valueonlineresource, valuetitle );
        Debug.debugMethodEnd();
        return instancevalue;
    }
    
    /**
     * returns an instance of the element &lt;valueOnLineResource&gt;<br>
     * <pre>
     * &lt;xs:sequence&gt;
     * &lt;xs:element name="linkage" type="iso19119:linkageType"/&gt;
     * &lt;xs:element ref="iso19119:protocol" minOccurs="0"/&gt;
     * &lt;xs:element ref="iso19119:applicationProfile" minOccurs="0"/&gt;
     * &lt;xs:element ref="iso19119:onlineResourceName" minOccurs="0"/&gt;
     * &lt;xs:element ref="iso19119:onlineResourceDescription" minOccurs="0"/&gt;
     * &lt;xs:element name="functionCode" type="iso19119:functionCodeType" minOccurs="0"/&gt;
     * &lt;/xs:sequence&gt;
     * </pre>
     */
    private static ValueOnLineResource getValueOnLineResource(Element volrElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getValueOnLineResource");
        
        // linkage
        Element element = XMLTools.getNamedChild(volrElement, NS119, "linkage");
        Linkage linkage = getLinkage(element);
        
        // protocol minOccurs="0"
        element = XMLTools.getNamedChild(volrElement, NS119, "protocol");
        String protocol = "";
        if(element != null) {
            protocol = element.getFirstChild().getNodeValue();
        }
        
        // applicationProfile minOccurs="0"
        element = XMLTools.getNamedChild(volrElement, NS119, "applicationProfile");
        String applicationprofile = "";
        if (element != null) {
            applicationprofile = element.getFirstChild().getNodeValue();
        }
        
        // onlineResourceName minOccurs="0"
        element = XMLTools.getNamedChild(volrElement, NS119, "onlineResourceName");
        String onlineresourcename = "";
        if (element != null) {
            onlineresourcename = element.getFirstChild().getNodeValue();
        }
        
        // onlineResourceDescription minOccurs="0"
        element = XMLTools.getNamedChild(volrElement, NS119, "onlineResourceDescription");
        String onlineresourcedescription = "";
        if (element != null) {
            onlineresourcedescription = element.getFirstChild().getNodeValue();
        }
        
        // functionCode minOccurs="0"
        element = XMLTools.getNamedChild(volrElement, NS119, "functionCode");
        FunctionCode functioncode = null;
        if (element != null) {
            functioncode = getFunctionCode(element);
        }
        
        ValueOnLineResource valueonlineresource = new ValueOnLineResource_Impl( applicationprofile, functioncode, linkage, onlineresourcedescription, onlineresourcename, protocol );
        
        Debug.debugMethodEnd();
        return valueonlineresource;
    }
    
    /**
     * returns an instance of the element &lt;value&gt;<br>
     */
    private static Value getValue(Element vElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getValue");
        
        String value = vElement.getAttribute("type");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("string")) ||
            (value.equals("number")) ) ) {
                throw new Exception("wrong value of the type-attribute: " + vElement);
            }
        } else throw new Exception("type attribute required! " + vElement);
        
        Value valuevalue = new Value_Impl( value );
        Debug.debugMethodEnd();
        return valuevalue;
    }
    
    
    /**
     * returns an instance of the element &lt;range&gt;<br>
     * xs:element name="minimumValue" type="iso19119:minimumValueType"
     * xs:element name="maximumValue" type="iso19119:maximumValueType"/
     */
    private static Range getRange(Element rElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getRange");
        
        Element element = XMLTools.getNamedChild( rElement, NS119, "minimumValue" );
        MinimumValue minimumvalue = getMinimumValue(element);
        
        element = XMLTools.getNamedChild( rElement, NS119, "maximumValue" );
        MaximumValue maximumvalue = getMaximumValue(element);
        
        Range range = new Range_Impl( maximumvalue, minimumvalue );
        
        Debug.debugMethodEnd();
        return range;
    }
    
    /**
     * returns an instance of the element &lt;minimumValue&gt;<br>
     * <pre>
     * <xs:sequence>
     * <xs:element ref="iso19119:valueTitle" minOccurs="0"/>
     * <xs:element ref="iso19119:valueDescription" minOccurs="0"/>
     * <xs:element name="valueOnLineResource" type="iso19119:valueOnLineResourceType" minOccurs="0"/>
     * <xs:element name="value" type="iso19119:valueType"/>
     * </xs:sequence>
     * </pre>
     */
    private static MinimumValue getMinimumValue(Element minElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getMinimumValue");
        
        // iso19119:valueTitle minOccurs="0"
        Element element = XMLTools.getNamedChild( minElement, NS119, "valueTitle");
        String valuetitle = "";
        if (element != null) {
            valuetitle = element.getFirstChild().getNodeValue();
        }
        
        // iso19119:valueDescription minOccurs="0"
        element = XMLTools.getNamedChild( minElement, NS119, "valueDescription");
        String valuedescription = "";
        if (element != null) {
            valuedescription = element.getFirstChild().getNodeValue();
        }
        
        // valueOnLineResource minOccurs="0"
        element = XMLTools.getNamedChild( minElement, NS119, "valueOnLineResource");
        ValueOnLineResource valueonlineresource = null;
        if (element != null) {
            valueonlineresource = getValueOnLineResource(element);
        }
        
        // value
        element = XMLTools.getNamedChild( minElement, NS119, "value");
        Value value = null;
        if (element != null) {
            value = getValue(element);
        }
        
        MinimumValue minimumvalue = new MinimumValue_Impl( value, valuedescription, valueonlineresource, valuetitle );
        Debug.debugMethodEnd();
        return minimumvalue;
    }
    
    
    /**
     * returns an instance of the element &lt;maximumValue&gt;<br>
     * @see getMinimumValue
     */
    private static MaximumValue getMaximumValue(Element maxElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getMaxnimumValue");
        
        // iso19119:valueTitle minOccurs="0"
        Element element = XMLTools.getNamedChild( maxElement, NS119, "valueTitle");
        String valuetitle = "";
        if (element != null) {
            valuetitle = element.getFirstChild().getNodeValue();
        }
        
        // iso19119:valueDescription minOccurs="0"
        element = XMLTools.getNamedChild( maxElement, NS119, "valueDescription");
        String valuedescription = "";
        if (element != null) {
            valuedescription = element.getFirstChild().getNodeValue();
        }
        
        // valueOnLineResource minOccurs="0"
        element = XMLTools.getNamedChild( maxElement, NS119, "valueOnLineResource");
        ValueOnLineResource valueonlineresource = null;
        if (element != null) {
            valueonlineresource = getValueOnLineResource(element);
        }
        
        // value
        element = XMLTools.getNamedChild( maxElement, NS119, "value");
        Value value = null;
        if (element != null) {
            value = getValue(element);
        }
        
        MaximumValue maximumvalue = new MaximumValue_Impl( value, valuedescription, valueonlineresource, valuetitle );
        Debug.debugMethodEnd();
        return maximumvalue;
    }
    
    /**
     * returns an instance of the element &lt;enumValues&gt;<br>
     * <pre>
     * <xs:sequence minOccurs="0" maxOccurs="unbounded">
     * <xs:sequence>
     * <xs:element ref="iso19119:valueTitle" minOccurs="0"/>
     * <xs:element ref="iso19119:valueDescription" minOccurs="0"/>
     * <xs:element name="valueOnLineResource" type="iso19119:valueOnLineResourceType" minOccurs="0"/>
     * <xs:element name="value" type="iso19119:valueType"/>
     * </xs:sequence>
     * </xs:sequence>
     * </pre>
     */
    private static EnumValues getEnumValues(Element evElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getEnumValues");
        
        // iso19119:valueTitle minOccurs="0"
        Element element = XMLTools.getNamedChild( evElement, NS119, "valueTitle");
        String valuetitle = "";
        if (element != null) {
            valuetitle = element.getFirstChild().getNodeValue();
        }
        
        // iso19119:valueDescription minOccurs="0"
        element = XMLTools.getNamedChild( evElement, NS119, "valueDescription");
        String valuedescription = "";
        if (element != null) {
            valuedescription = element.getFirstChild().getNodeValue();
        }
        
        // valueOnLineResource minOccurs="0"
        element = XMLTools.getNamedChild( evElement, NS119, "valueOnLineResource");
        ValueOnLineResource valueonlineresource = null;
        if (element != null) {
            valueonlineresource = getValueOnLineResource(element);
        }
        
        // value
        element = XMLTools.getNamedChild( evElement, NS119, "value");
        Value value = null;
        if (element != null) {
            value = getValue(element);
        }
        
        EnumValues enumvalues = new EnumValues_Impl( value, valuedescription, valueonlineresource, valuetitle );
        Debug.debugMethodEnd();
        return enumvalues;
    }
    
    
    
    
    // ########################################################################
    
    
    /**
     * returns an instance of the element &lt;legalConstraints&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static LegalConstraints getLegalConstraints(Element lcElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getLegalConstraints" );
        
        // useLimitation minOccurs="0" maxOccurs="unbounded"
        NodeList nl = lcElement.getElementsByTagNameNS(NS119, "useLimitation");
        ArrayList uselimitationlist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                uselimitationlist.add(i, nl.item(i).getFirstChild().getNodeValue());
            }
        }
        String[] uselimitation = (String[])uselimitationlist.toArray( new String[uselimitationlist.size()] );
        
        // propertyRightsCode minOccurs="0" maxOccurs="unbounded"
        nl = lcElement.getElementsByTagNameNS(NS119, "propertyRightsCode");
        ArrayList propertyrightscodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                propertyrightscodelist.add(i, getPropertyRightsCode((Element)nl.item(i)));
            }
        }
        PropertyRightsCode[] propertyrightscode = (PropertyRightsCode[])propertyrightscodelist.toArray( new PropertyRightsCode[propertyrightscodelist.size()] );

        // name="useConstraintsCode" minOccurs="0" maxOccurs="unbounded"
        nl = lcElement.getElementsByTagNameNS(NS119, "useConstraintsCode");
        ArrayList useconstraintscodelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                useconstraintscodelist.add(i, getUseConstraintsCode((Element)nl.item(i)));
            }
        }
        UseConstraintsCode[] useconstraintscode = (UseConstraintsCode[])useconstraintscodelist.toArray( new UseConstraintsCode[useconstraintscodelist.size()] );

        // ref="iso19119:otherConstraints" minOccurs="0" maxOccurs="unbounded"
        nl = lcElement.getElementsByTagNameNS(NS119, "otherConstraints");
        ArrayList otherconstraintslist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                otherconstraintslist.add(i, nl.item(i).getFirstChild().getNodeValue());
            }
        }
        String[] otherconstraints = (String[])otherconstraintslist.toArray( new String[otherconstraintslist.size()] );
        
        LegalConstraints legalconstraints = new LegalConstraints_Impl( otherconstraints, propertyrightscode, useconstraintscode, uselimitation );
        Debug.debugMethodEnd();
        return legalconstraints;
    }
    
    /**
     * returns an instance of the element &lt;propertyRightsCode&gt;<br>
     * minOccurs="0" maxOccurs="unbounded"
     */
    private static PropertyRightsCode getPropertyRightsCode(Element prcElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getPropertyRightsCode");
        
        String value = prcElement.getAttribute("Restrict");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("copyright")) ||
            (value.equals("patent")) ||
            (value.equals("patentPending")) ||
            (value.equals("license")) ||
            (value.equals("intellectualPropertyRights")) ||
            (value.equals("trademark")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + prcElement);
            }
        } else throw new Exception("value attribute required! " + prcElement);
        
        PropertyRightsCode propertyrightscode = new PropertyRightsCode_Impl( value );
        
        Debug.debugMethodEnd();
        return propertyrightscode;
    }
    
    /**
     * returns an instance of the element &lt;useConstraintsCode&gt;<br>
     * minOccurs="0" maxOccurs="unbounded"
     */
    private static UseConstraintsCode getUseConstraintsCode(Element uccElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getUseConstraintsCode");
        
        String value = uccElement.getAttribute("Restrict");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("copyright")) ||
            (value.equals("patent")) ||
            (value.equals("patentPending")) ||
            (value.equals("license")) ||
            (value.equals("intellectualPropertyRights")) ||
            (value.equals("trademark")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + uccElement);
            }
        } else throw new Exception("value attribute required! " + uccElement);
        
        UseConstraintsCode useconstraintscode = new UseConstraintsCode_Impl( value );
        
        Debug.debugMethodEnd();
        return useconstraintscode;
    }
    
    
    /**
     * returns an instance of the element &lt;securityConstraints&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static SecurityConstraints getSecurityConstraints(Element scElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getSecurityConstraints" );
        
        // iso19119:useLimitation minOccurs="0" maxOccurs="unbounded"
        // useLimitation minOccurs="0" maxOccurs="unbounded"
        NodeList nl = scElement.getElementsByTagNameNS(NS119, "useLimitation");
        ArrayList uselimitationlist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                uselimitationlist.add(i, nl.item(i).getFirstChild().getNodeValue());
            }
        }
        String[] uselimitation = (String[])uselimitationlist.toArray( new String[uselimitationlist.size()] );
        
	// classificationCode type="iso19119:classificationCodeType"
        Element element = XMLTools.getNamedChild( scElement, NS119, "classificationCode");
        ClassificationCode classificationcode = getClassificationCode(element);
        
	// iso19119:userNote minOccurs="0"
        element = XMLTools.getNamedChild( scElement, NS119, "userNote");
        String usernote = "";
        if (element != null) {
            usernote = element.getFirstChild().getNodeValue();
        }
        
	// iso19119:classificationSystem minOccurs="0"
        element = XMLTools.getNamedChild( scElement, NS119, "classificationSystem");
        String classificationsystem = "";
        if (element != null) {
            classificationsystem = element.getFirstChild().getNodeValue();
        }
        
	// iso19119:handlingDescription minOccurs="0"
        element = XMLTools.getNamedChild( scElement, NS119, "handlingDescription");
        String handlingdescription = "";
        if (element != null) {
            handlingdescription = element.getFirstChild().getNodeValue();
        }
        
        SecurityConstraints securityconstraints = new SecurityConstraints_Impl( classificationcode, classificationsystem, handlingdescription, uselimitation, usernote );
        Debug.debugMethodEnd();
        return securityconstraints;
    }
    
    /**
     * returns an instance of the element &lt;classificationCode&gt;
     */
    private static ClassificationCode getClassificationCode(Element ccElement) throws Exception {
        Debug.debugMethodBegin("WCAS_ISO19119Factory", "getClassificationCode");
        
        String value = ccElement.getAttribute("Classification");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("unclassified")) ||
            (value.equals("codeWord")) ||
            (value.equals("confidential")) ||
            (value.equals("secret")) ||
            (value.equals("restricted")) ||
            (value.equals("topsecret")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + ccElement);
            }
        } else throw new Exception("value attribute required! " + ccElement);
        
        ClassificationCode classificationcode = new ClassificationCode_Impl( value );
        
        Debug.debugMethodEnd();
        return classificationcode;
    }
    
    
    /**
     * returns an instance of the element &lt;quality&gt;<br>
     * <tt>minOccurs="0"</tt><br>
     * <pre>
       <xs:complexType name="qualityType">
         <xs:sequence>
           <xs:element ref="iso19119:TBD_ServiceQuality"/>
         </xs:sequence>
       </xs:complexType>
     * </pre>
     */
    private static Quality getQuality(Element qElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getQuality" );
        
        NodeList nl = qElement.getElementsByTagNameNS( NS119, "TBD_ServiceQuality" );
        ArrayList tbdservicequalitylist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                tbdservicequalitylist.add(i, nl.item(i).getFirstChild().getNodeValue());
            }
        }
        String[] tbd_servicequality = (String[])tbdservicequalitylist.toArray( new String[tbdservicequalitylist.size()] );
        
        Quality quality = new Quality_Impl( tbd_servicequality );
        
        Debug.debugMethodEnd();
        return quality;
    }
    
    /**
     * returns an instance of the element &lt;operationMetadata&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static OperationMetadata getOperationMetadata(Element omElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getOperationMetadata" );
        
        // operationName
        Element element = XMLTools.getNamedChild( omElement, NS119, "operationName");
        OperationName operationname = getOperationName(element);

        // iso19119:operationDescription
        element = XMLTools.getNamedChild( omElement, NS119, "operationDescription");
        String operationdescription = element.getFirstChild().getNodeValue();
        
        // parameter minOccurs="0" maxOccurs="unbounded"
        NodeList nl = omElement.getElementsByTagNameNS( NS119, "parameter" );
        ArrayList parameterlist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                parameterlist.add(i, getParameter((Element)nl.item(i)));
            }
        }
        Parameter[] parameter = (Parameter[])parameterlist.toArray( new Parameter[parameterlist.size()] );
        
        // dependsOn minOccurs="0"
        element = XMLTools.getNamedChild(omElement, NS119, "dependsOn");
        DependsOn dependson = getDependsOn(element);
        
        // DCP maxOccurs="unbounded"
        nl = omElement.getElementsByTagNameNS( NS119, "DCP" );
        ArrayList dcplist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                dcplist.add(i, getDcp((Element)nl.item(i)));
            }
        }
        Dcp[] dcp = (Dcp[])dcplist.toArray( new Dcp[dcplist.size()] );
        
        OperationMetadata operationmetadata = new OperationMetadata_Impl( dcp, dependson, operationdescription, operationname, parameter );
      
        Debug.debugMethodEnd();
        return operationmetadata;
    }
    
    /**
     * returns an instance of the element &lt;operationMetadata&gt;
     */
    private static OperationName getOperationName(Element onElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getOperationMetadata" );
        
        // iso19119:nameValue
        Element element = XMLTools.getNamedChild(onElement, NS119, "nameValue");
        String namevalue = element.getFirstChild().getNodeValue();

        // iso19119:nameNameSpace
        element = XMLTools.getNamedChild(onElement, NS119, "nameNameSpace");
        String namenamespace = element.getFirstChild().getNodeValue();
        
        OperationName operationname = new OperationName_Impl( namenamespace, namevalue );
        
        Debug.debugMethodEnd();
        return operationname;
    }
    
    /**
     * returns an instance of the element &lt;parameter&gt;
     */
    private static Parameter getParameter(Element pElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getParameter" );
        // ATTRIBUTES
        // optional use="required" values: "yes" or "no"
        String optional = pElement.getAttribute("optional");
        if( optional.length() > 0 ) {
            // negotiation of the required values
            if ( !( (optional.equals("yes")) ||
                    (optional.equals("no")) ) ) {
                throw new Exception("wrong value of the optional-attribute: " + pElement);
            }
        } else throw new Exception("optional attribute required! " + pElement);
        
        // repeatable use="required" values: "true" or "false"
        String repeatable = pElement.getAttribute("repeatable");
        if( repeatable.length() > 0 ) {
            // negotiation of the required values
            if ( !( (repeatable.equals("true")) ||
                    (repeatable.equals("false")) ) ) {
                throw new Exception("wrong value of the repeatable-attribute: " + pElement);
            }
        } else throw new Exception("repeatable attribute required! " + pElement);
                
	// direction use="required" values: "in" or "out" or "inout"
        String direction = pElement.getAttribute("direction");
        if( direction.length() > 0 ) {
            // negotiation of the required values
            if ( !( (direction.equals("in")) ||
                    (direction.equals("out")) ||
                    (direction.equals("inout")) ) ) {
                throw new Exception("wrong value of the direction-attribute: " + pElement);
            }
        } else throw new Exception("direction attribute required! " + pElement);
        
        // ELEMENTS
        // parameterName
        Element element = XMLTools.getNamedChild( pElement, NS119, "parameterName");
        ParameterName parametername = getParameterName( element );
        
	// parameterType
        element = XMLTools.getNamedChild( pElement, NS119, "parameterType");
        ParameterType parametertype = getParameterType( element );
        
	// parameterDescription minOccurs="0"
        element = XMLTools.getNamedChild( pElement, NS119, "parameterDescription");
        String parameterdescription = "";
        if (element != null) {
            parameterdescription = element.getFirstChild().getNodeValue();
        }

	// permittedValues
        element = XMLTools.getNamedChild( pElement, NS119, "permittedValues");
        PermittedValues permittedvalues = getPermittedValues( element );

        Parameter parameter = new org.deegree_impl.services.wcas.metadatadesc.Parameter_Impl( direction, optional, parameterdescription, parametername, parametertype, permittedvalues, repeatable);
        Debug.debugMethodEnd();
        return parameter;
    }
    
    /**
     * returns an instance of the element &lt;parameterName&gt;
     */
    private static ParameterName getParameterName(Element pnElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getParameterName" );
        
        // element nameValue
        Element element = XMLTools.getNamedChild(pnElement, NS119, "nameValue");
        String namevalue = element.getFirstChild().getNodeValue();
        
        // element nameNameSpace
        element = XMLTools.getNamedChild(pnElement, NS119, "nameNameSpace");
        String namenamespace = element.getFirstChild().getNodeValue();
        
        ParameterName parametername = new ParameterName_Impl(namenamespace, namevalue);
        Debug.debugMethodEnd();
        return parametername;
    }
    
    /**
     * returns an instance of the element &lt;parameterType&gt;
     */
    private static ParameterType getParameterType(Element ptElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getParameterType" );
        
        String value = XMLTools.getAttrValue(ptElement, "type");

        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("string")) ||
            (value.equals("number")) ) ) {
                throw new Exception("wrong value of the type-attribute: " + ptElement);
            }
        } else throw new Exception("type attribute required! " + ptElement);
        
        ParameterType parametertype = new ParameterType_Impl( value );

        Debug.debugMethodEnd();
        return parametertype;
    }
    
    /**
     * returns an instance of the element &lt;permittedValues&gt;<br>
     * Element onLineResource<br>
     * and<br>
     * choice (minOccurs="0" maxOccurs="unbounded"): dataType, instanceValue, range, enumValues
     */
    private static PermittedValues getPermittedValues(Element pvElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getPermittedValues" );
        
        // onLineResource minOccurs="0"
        Element element = XMLTools.getNamedChild(pvElement, NS119, "onLineResource");
        OnLineResource onlineresource = null;
        if (element != null) {
            onlineresource = getOnLineResource(element);
        }
        
        // dataType
        NodeList nl = pvElement.getElementsByTagNameNS( NS119, "dataType" );
        ArrayList datatypelist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                datatypelist.add(i, getDataType((Element)nl.item(i)));
            }
        }
        DataType[] datatype = (DataType[])datatypelist.toArray( new DataType[datatypelist.size()] );
        
        // instanceValue
        nl = pvElement.getElementsByTagNameNS( NS119, "instanceValue" );
        ArrayList instancevaluelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                instancevaluelist.add(i, getInstanceValue((Element)nl.item(i)));
            }
        }
        InstanceValue[] instancevalue = (InstanceValue[])instancevaluelist.toArray( new InstanceValue[instancevaluelist.size()] );
        
        // range
        nl = pvElement.getElementsByTagNameNS( NS119, "range" );
        ArrayList rangelist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                rangelist.add(i, getRange((Element)nl.item(i)));
            }
        }
        Range[] range = (Range[])rangelist.toArray( new Range[rangelist.size()] );
        
        // enumValues
        nl = pvElement.getElementsByTagNameNS( NS119, "enumValues" );
        ArrayList enumvalueslist = new ArrayList();
        length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                enumvalueslist.add(i, getEnumValues((Element)nl.item(i)));
            }
        }
        EnumValues[] enumvalues = (EnumValues[])enumvalueslist.toArray( new EnumValues[enumvalueslist.size()] );
        
        PermittedValues permittedvalues = new PermittedValues_Impl( datatype, enumvalues, instancevalue, onlineresource, range );
        Debug.debugMethodEnd();
        return permittedvalues;
    }
    
    /**
     * returns an instance of the element &lt;dependsOn&gt;<br>
     * complexType xs:sequence xs:element name="operationName" minOccurs="0" maxOccurs="unbounded"
     */
    private static DependsOn getDependsOn(Element doElement) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getDependsOn" );

        NodeList nl = doElement.getElementsByTagNameNS( NS119, "operationName" );
        ArrayList operationnamelist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                operationnamelist.add(i, getOperationName((Element)nl.item(i)));
            }
        }
        OperationName[] operationname = (OperationName[])operationnamelist.toArray( new OperationName[operationnamelist.size()] );
        
        DependsOn dependson = new DependsOn_Impl(operationname);
        Debug.debugMethodEnd();
        return dependson;
    }
    
    /**
     * returns an instance of the element &lt;DCP&gt;<br>
     * elements: "invocationName", "connectPoint", "parameter" minOccurs="0" maxOccurs="unbounded"<br>
     * attributes: name="type" use="required" - values: "HTTPGet", "HTTPPost"
     */
    private static Dcp getDcp(Element dcpElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getDcp" );
        
        // ATTRIBUTES
        String type = dcpElement.getAttribute("type");
        if( type.length() > 0 ) {
            if ( !( (type.equals("HTTPGet")) ||
                    (type.equals("HTTPPost")) ) ) {
                throw new Exception("wrong value of the optional-attribute: " + dcpElement);
            }
        } else throw new Exception("optional attribute required! " + dcpElement);
        
        // invocationName
        Element element = XMLTools.getNamedChild(dcpElement, NS119, "invocationName");
        String invocationname = element.getFirstChild().getNodeValue();
        
        // connectPoint
        element = XMLTools.getNamedChild(dcpElement, NS119, "connectPoint");
        ConnectPoint connectpoint = getConnectPoint( element );
        
        // parameter minOccurs="0" maxOccurs="unbounded"
        NodeList nl = dcpElement.getElementsByTagNameNS( NS119, "parameter" );
        ArrayList parameterlist = new ArrayList();
        int length = nl.getLength();
        if (nl != null && length > 0) {
            for (int i = 0; i < length; i++) {
                parameterlist.add(i, getParameter((Element)nl.item(i)));
            }
        }
        Parameter[] parameter = (Parameter[])parameterlist.toArray( new Parameter[parameterlist.size()] );

        Dcp dcp = new Dcp_Impl( connectpoint, invocationname, parameter, type ); 

        Debug.debugMethodEnd();
        return dcp;
    }
    
    /**
     * returns an instance of the element &lt;connectPoint&gt;<br>
     * elements: linkage,
     * protocol minOccurs="0",
     * applicationProfile minOccurs="0",
     * onlineResourceName minOccurs="0",
     * onlineResourceDescription minOccurs="0",
     * functionCode minOccurs="0"
     */
    private static ConnectPoint getConnectPoint(Element cpElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getConnectPoint" );
        
        // linkage
        Element element = XMLTools.getNamedChild(cpElement, NS119, "linkage");
        Linkage linkage = getLinkage(element);
        
        // protocol minOccurs="0",
        element = XMLTools.getNamedChild(cpElement, NS119, "protocol");
        String protocol = "";
        if (element != null) {
            protocol = element.getFirstChild().getNodeValue();
        }
        
        // applicationProfile minOccurs="0"
        element = XMLTools.getNamedChild(cpElement, NS119, "applicationProfile");
        String applicationprofile = "";
        if (element != null) {
            applicationprofile = element.getFirstChild().getNodeValue();
        }
        
        // onlineResourceName minOccurs="0"
        element = XMLTools.getNamedChild(cpElement, NS119, "onlineResourceName");
        String onlineresourcename = "";
        if (element != null) {
            onlineresourcename = element.getFirstChild().getNodeValue();
        }
        
        // onlineResourceDescription minOccurs="0"
        element = XMLTools.getNamedChild(cpElement, NS119, "onlineResourceDescription");
        String onlineresourcedescription = "";
        if (element != null) {
            onlineresourcedescription = element.getFirstChild().getNodeValue();
        }
        
        // functionCode minOccurs="0"
        element = XMLTools.getNamedChild(cpElement, NS119, "functionCode");
        FunctionCode functioncode = null;
        if (element != null) {
            functioncode = getFunctionCode( element );
        }
        
        ConnectPoint connectpoint = new ConnectPoint_Impl( applicationprofile, functioncode, linkage, onlineresourcedescription, onlineresourcename, protocol );
        Debug.debugMethodEnd();
        return connectpoint;
    }

    
    /**
     * returns an instance of the element &lt;dataCoupling&gt;
     * <pre>
     * &lt;xs:complexType name="dataCouplingType"&gt;
     * &lt;xs:attribute name="couplingType" use="required"&gt;
     * &lt;xs:simpleType&gt;
     * &lt;xs:restriction base="xs:NMTOKEN"&gt;
     * &lt;xs:enumeration value="tight"/&gt;
     * &lt;xs:enumeration value="loose"/&gt;
     * &lt;xs:enumeration value="mixed"/&gt;
     * &lt;/xs:restriction&gt;
     * &lt;/xs:simpleType&gt;
     * &lt;/xs:attribute&gt;
     * &lt;/xs:complexType&gt;
     * </pre>
     */
    private static DataCoupling getDataCoupling(Element dcElement) throws Exception {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getDataCoupling" );
        
        String value = dcElement.getAttribute("couplingType");
        
        if( value.length() > 0 ) {
            // negotiation of the required values
            if ( !( (value.equals("tight")) ||
            (value.equals("loose")) ||
            (value.equals("mixed")) ) ) {
                throw new Exception("wrong value of the value-attribute: " + dcElement);
            }
        } else throw new Exception("value attribute required! " + dcElement);
        
        DataCoupling datacoupling = new DataCoupling_Impl( value );
        
        Debug.debugMethodEnd();
        return datacoupling;
    }
    
    /**
     * returns an instance of the the MD_Metadata, the element
     * &lt;ISO_19115&gt;<br>
     * <tt>minOccurs="0" maxOccurs="unbounded"</tt>
     */
    private static ISO19115[] getMD_Metadata(Element element) {
        Debug.debugMethodBegin( "WCAS_ISO19119Factory", "getMD_Metadata" );

        ISO19115[] md_metadata = new ISO19115[1];
        md_metadata[0] = new ISO19115_Impl();

        Debug.debugMethodEnd();

        return md_metadata;
    }
}
