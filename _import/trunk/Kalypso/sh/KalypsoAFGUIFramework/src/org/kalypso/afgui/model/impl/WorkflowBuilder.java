/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.kalypso.afgui.model.EActivityRelationship;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IRelationshipStatement;
import org.kalypso.afgui.model.IWorkflowRuntineStatus;
import org.kalypso.afgui.model.IWorkflow;
import org.kalypso.afgui.model.IWorkflowBuilder;
import org.kalypso.afgui.model.IWorkflowSpecification;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import com.hp.hpl.jena.vocabulary.RDF;


/**
 * 
 * @author Patrice Congo
 */
public class WorkflowBuilder implements IWorkflowBuilder
{
	final static private Logger logger=
				Logger.getLogger(WorkflowBuilder.class);
	
	
	public IWorkflow createWorkflow(URL specURL, URL statusURL) throws IOException
	{
		if(specURL==null)
		{
			throw new IllegalArgumentException(
					"A spec url is needed to create a workspace");
		}
		
		return new WorkflowImpl(
				loadSpecifications(specURL),
				loadRuntimeStatus(statusURL)); 
	}
	
	
	
	private final IWorkflowSpecification loadSpecifications(URL specURL) throws IOException
	{
		logger.info("specURL="+specURL);
		IWorkflowSpecification wfSpec=null;
		Map<String, IActivitySpecification> specsMap=
			new HashMap<String, IActivitySpecification>();
		List<IRelationshipStatement> stms= new ArrayList<IRelationshipStatement>();
		//model load
		InputStream iStream=specURL.openStream();
		Model rdfModel= ModelFactory.createDefaultModel();
		rdfModel.read(iStream,"");
		rdfModel.write(System.out);
		
		//fill map
//		Selector selectActivities= 
//			new SimpleSelector((Resource)null,RDF.type,RDFS.Resource);//Schema.URI_CLASS_ACTIVITY)
//		
		IActivitySpecification spec=null;
//		ResIterator it=
//			rdfModel.listSubjectsWithProperty(RDF.type, Schema.URI_CLASS_ACTIVITY);
//		logger.info("\nHAS_NEXT:"+it.hasNext());
//		logger.info("ActivityURI"+Schema.URI_CLASS_ACTIVITY);
//		
//		for(;it.hasNext();)
//		{
//			spec= new ActivitySpecification(it.nextResource());
//			logger.info(spec.toString());
//			specsMap.put(spec.getName(), spec);
//		}
		
		StmtIterator it=
			rdfModel.listStatements((Resource)null,RDF.type,Schema.CLASS_ACTIVITY);//selectActivities);
		logger.info("\nHAS_NEXT:"+it.hasNext());
		Statement stm;		
		for(;it.hasNext();)
		{
			stm=it.nextStatement();
			spec= new ActivitySpecification(stm.getSubject());
			logger.info(stm.getPredicate());
			specsMap.put(spec.getID(), spec);
			
		}
		
		//create statement list, get activity link statements, 
		//construct statement based on it
		IActivitySpecification objSpec=null;
		IActivitySpecification subSpec=null;
		IRelationshipStatement relStm=null;
		EActivityRelationship rel;
		
		for(Property prop:Schema.ACTIVITY_LINK_PROPS)
		{
			it=
				rdfModel.listStatements((Resource)null,prop,(Resource)null);
			for(;it.hasNext();)
			{
				stm=it.nextStatement();
				subSpec= specsMap.get(stm.getSubject().getURI());
				objSpec= specsMap.get(((Resource)stm.getObject()).getURI());
				rel=Schema.toEActivityRelationship(prop);
				if(subSpec==null || objSpec==null|| rel==null)
				{
					logger.warn("RelationTripple=["+subSpec+", "+rel+"/"+prop+", "+objSpec);
				}
				else
				{
					relStm= new RelationshipStatement(subSpec,rel,objSpec);
					stms.add(relStm);
					if(rel==EActivityRelationship.PART_OF)
					{
						IRelationshipStatement inverseRelStm=
							new RelationshipStatement(
									objSpec,EActivityRelationship.HAS_A,subSpec);
						if(!stms.contains(inverseRelStm))
						{
							stms.add(inverseRelStm);
						}
					}
					else if(rel==EActivityRelationship.HAS_A)
					{
						IRelationshipStatement inverseRelStm=
							new RelationshipStatement(
									objSpec,EActivityRelationship.PART_OF,subSpec);
						if(!stms.contains(inverseRelStm))
						{
							stms.add(inverseRelStm);
						}
					}
				}
				
			}
		}
		logger.info(stms);
		//create workflow spec
		wfSpec= new WorkflowSpecification(specsMap,stms);
		return wfSpec;
	}
	
	



	private final IWorkflowRuntineStatus loadRuntimeStatus(URL specURL)
	{
		IWorkflowRuntineStatus wfStatus=null;
		wfStatus=new WorkflowRuntimeStatus();
		return wfStatus;
	}
}
