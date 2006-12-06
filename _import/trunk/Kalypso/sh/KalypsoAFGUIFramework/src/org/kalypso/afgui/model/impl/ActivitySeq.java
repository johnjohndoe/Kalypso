package org.kalypso.afgui.model.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;


import org.kalypso.afgui.model.IActivity;
import org.kalypso.afgui.model.IActivitySeq;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

//TODO replace with generic type
public class ActivitySeq implements IActivitySeq
{
	final private Seq seq;
	
	public ActivitySeq(Seq seq)
	{
		this.seq=seq;
	}
	
	public ActivitySeq(Resource resource)
	{
		if(resource instanceof Seq)
		{
			this.seq=(Seq)resource;
		}
		else
		{
			throw new IllegalArgumentException("Resource not a sequence:"+resource);
		}
	}

	public ActivitySeq(String uri, Model model)
	{
		if(model==null)
		{
			throw new IllegalArgumentException();
		}
		
		seq= Schema.createActivitySeq(uri, model);
	}

	public boolean add(IActivity o)
	{
		if(o==null)
		{
			return false;
		}
		else
		{
			Resource res= (Resource)o.getModelObject();
			if(res==null)
			{
				return false;
			}
			else
			{
				seq.add(res);
				return true;
			}
		}
	}

	public void add(int index, IActivity element)
	{
		
		if(element==null)
		{
			return;
		}
		else if(index>=size())
		{
			throw new ArrayIndexOutOfBoundsException(index);
		}			
		else
		{
			Resource res= (Resource)element.getModelObject();
			if(res==null)
			{
				throw new IllegalArgumentException("No resource wrapped in element");
			}
			else
			{
				seq.add(res);
				return;
			}
		}
	}

	public boolean addAll(Collection<? extends IActivity> c)
	{
		//CHECK COLLECTION
		if(c==null)
		{
			throw new NullPointerException("Collection to add must not be null");
		}
		//check element as 
		for(IActivity a:c)
		{
			
		}
		return false;
	}

	public boolean addAll(int index, Collection<? extends IActivity> c)
	{
		return false;
	}

	public void clear()
	{
		
	}

	public boolean contains(Object o)
	{
		return false;
	}

	public boolean containsAll(Collection<?> c)
	{
		return false;
	}

	public IActivity get(int index)
	{
		return new Activity(seq.getResource(index+1));
	}

	public int indexOf(Object o)
	{
		return 0;
	}

	public boolean isEmpty()
	{
		return false;
	}

	public Iterator<IActivity> iterator()
	{
		
		Iterator<IActivity> it=new Iterator<IActivity>()
		{
			final  NodeIterator nIt=seq.iterator();
			public boolean hasNext()
			{
				return nIt.hasNext();
			}

			public IActivity next()
			{
				return new Activity((Resource)nIt.next());
			}

			public void remove()
			{
				nIt.remove();
			}
			
		};
		return it;
	}

	public int lastIndexOf(Object o)
	{
		return 0;
	}

	public ListIterator<IActivity> listIterator()
	{
//		ListIterator< IActivity> lIt= new ListIterator<IActivity>()
//		{
//			private int currentIndex=0;
//			
//			public void add(IActivity o)
//			{
//				// TODO Auto-generated method stub				
//			}
//
//			public boolean hasNext()
//			{
//				return currentIndex<size();
//			}
//
//			public boolean hasPrevious()
//			{
//				return currentIndex>0;
//			}
//
//			public IActivity next()
//			{
//				return ActitivitySeq.this.get(currentIndex);
//			}
//
//			public int nextIndex()
//			{				
//				return currentIndex;
//			}
//
//			public IActivity previous()
//			{
//				currentIndex--;
//				return new Activity(seq.getResource(currentIndex));
//			}
//
//			public int previousIndex()
//			{				
//				return currentIndex-1;
//			}
//
//			public void remove()
//			{
//				throw new UnsupportedOperationException();
//			}
//
//			public void set(IActivity o)
//			{
//				ActitivitySeq.this.set(currentIndex, o);
//			}
//			
//		};
//		return null;
		throw new UnsupportedOperationException();
	}

	public ListIterator<IActivity> listIterator(int index)
	{
		throw new UnsupportedOperationException();
	}

	public boolean remove(Object o)
	{
		if(o==null)
		{
			return false;
		}
		else
		{
			if(o instanceof IActivity)
			{
				Resource res=(Resource)((IActivity)o).getModelObject();
				if(res==null)
				{
					return false;
				}
				else
				{
					int index=seq.indexOf(res);
					if(index==0)
					{
						return false;
					}
					else
					{
						seq.remove(index);
						return true;
					}
				}
			}
			else
			{
				return false;
			}
		}
	}

	public IActivity remove(int index)
	{
		if(index<size())
		{
			IActivity a=get(index);
			return a;
		}
		else
		{
			throw new IndexOutOfBoundsException();
		}
	}

	public boolean removeAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public IActivity set(int index, IActivity element)
	{
		if(index<size())
		{
			throw new IndexOutOfBoundsException();
		}
		if(element==null)
		{
			throw new NullPointerException();
		}
		
		Resource res= (Resource)element.getModelObject();
		if(res==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			return null;
		}
	}

	public int size()
	{
		return seq.size();
	}

	public List<IActivity> subList(int fromIndex, int toIndex)
	{
		//return null;
		throw new UnsupportedOperationException();
	}

	public Object[] toArray()
	{
		int size= size();
		if(size==0)
		{
			return new IActivity[]{};
		}
		else
		{
			IActivity[] as=new IActivity[size];
			return toArray(as);
		}
	}

	public <T> T[] toArray(T[] a)
	{
		if(a==null)
		{
			throw new NullPointerException();
		}
		else
		{
			if(size()>a.length)
			{
				throw new IllegalArgumentException();
			}
			
			if(	a instanceof IActivity[]||
				a instanceof Object[])
			{
				
				for(int i=0;i<a.length;i++)
				{
					a[i]=(T)get(i);
				}
				return a;
			}
			else
			{
				throw new UnsupportedOperationException();
			}
		}
	}

	@Override
	public boolean equals(Object obj)
	{
		if(obj instanceof ActivitySeq)
		{
			try
			{
				return  seq.equals(((ActivitySeq)obj).seq);
			}
			catch(Throwable th)
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}
	

}
