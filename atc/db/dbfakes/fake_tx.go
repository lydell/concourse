// Code generated by counterfeiter. DO NOT EDIT.
package dbfakes

import (
	"context"
	"database/sql"
	"sync"

	"github.com/Masterminds/squirrel"
	"github.com/concourse/concourse/atc/db"
	"github.com/concourse/concourse/atc/db/encryption"
)

type FakeTx struct {
	CommitStub        func() error
	commitMutex       sync.RWMutex
	commitArgsForCall []struct {
	}
	commitReturns struct {
		result1 error
	}
	commitReturnsOnCall map[int]struct {
		result1 error
	}
	EncryptionStrategyStub        func() encryption.Strategy
	encryptionStrategyMutex       sync.RWMutex
	encryptionStrategyArgsForCall []struct {
	}
	encryptionStrategyReturns struct {
		result1 encryption.Strategy
	}
	encryptionStrategyReturnsOnCall map[int]struct {
		result1 encryption.Strategy
	}
	ExecStub        func(string, ...interface{}) (sql.Result, error)
	execMutex       sync.RWMutex
	execArgsForCall []struct {
		arg1 string
		arg2 []interface{}
	}
	execReturns struct {
		result1 sql.Result
		result2 error
	}
	execReturnsOnCall map[int]struct {
		result1 sql.Result
		result2 error
	}
	ExecContextStub        func(context.Context, string, ...interface{}) (sql.Result, error)
	execContextMutex       sync.RWMutex
	execContextArgsForCall []struct {
		arg1 context.Context
		arg2 string
		arg3 []interface{}
	}
	execContextReturns struct {
		result1 sql.Result
		result2 error
	}
	execContextReturnsOnCall map[int]struct {
		result1 sql.Result
		result2 error
	}
	PrepareStub        func(string) (*sql.Stmt, error)
	prepareMutex       sync.RWMutex
	prepareArgsForCall []struct {
		arg1 string
	}
	prepareReturns struct {
		result1 *sql.Stmt
		result2 error
	}
	prepareReturnsOnCall map[int]struct {
		result1 *sql.Stmt
		result2 error
	}
	PrepareContextStub        func(context.Context, string) (*sql.Stmt, error)
	prepareContextMutex       sync.RWMutex
	prepareContextArgsForCall []struct {
		arg1 context.Context
		arg2 string
	}
	prepareContextReturns struct {
		result1 *sql.Stmt
		result2 error
	}
	prepareContextReturnsOnCall map[int]struct {
		result1 *sql.Stmt
		result2 error
	}
	QueryStub        func(string, ...interface{}) (*sql.Rows, error)
	queryMutex       sync.RWMutex
	queryArgsForCall []struct {
		arg1 string
		arg2 []interface{}
	}
	queryReturns struct {
		result1 *sql.Rows
		result2 error
	}
	queryReturnsOnCall map[int]struct {
		result1 *sql.Rows
		result2 error
	}
	QueryContextStub        func(context.Context, string, ...interface{}) (*sql.Rows, error)
	queryContextMutex       sync.RWMutex
	queryContextArgsForCall []struct {
		arg1 context.Context
		arg2 string
		arg3 []interface{}
	}
	queryContextReturns struct {
		result1 *sql.Rows
		result2 error
	}
	queryContextReturnsOnCall map[int]struct {
		result1 *sql.Rows
		result2 error
	}
	QueryRowStub        func(string, ...interface{}) squirrel.RowScanner
	queryRowMutex       sync.RWMutex
	queryRowArgsForCall []struct {
		arg1 string
		arg2 []interface{}
	}
	queryRowReturns struct {
		result1 squirrel.RowScanner
	}
	queryRowReturnsOnCall map[int]struct {
		result1 squirrel.RowScanner
	}
	QueryRowContextStub        func(context.Context, string, ...interface{}) squirrel.RowScanner
	queryRowContextMutex       sync.RWMutex
	queryRowContextArgsForCall []struct {
		arg1 context.Context
		arg2 string
		arg3 []interface{}
	}
	queryRowContextReturns struct {
		result1 squirrel.RowScanner
	}
	queryRowContextReturnsOnCall map[int]struct {
		result1 squirrel.RowScanner
	}
	RollbackStub        func() error
	rollbackMutex       sync.RWMutex
	rollbackArgsForCall []struct {
	}
	rollbackReturns struct {
		result1 error
	}
	rollbackReturnsOnCall map[int]struct {
		result1 error
	}
	StmtStub        func(*sql.Stmt) *sql.Stmt
	stmtMutex       sync.RWMutex
	stmtArgsForCall []struct {
		arg1 *sql.Stmt
	}
	stmtReturns struct {
		result1 *sql.Stmt
	}
	stmtReturnsOnCall map[int]struct {
		result1 *sql.Stmt
	}
	invocations      map[string][][]interface{}
	invocationsMutex sync.RWMutex
}

func (fake *FakeTx) Commit() error {
	fake.commitMutex.Lock()
	ret, specificReturn := fake.commitReturnsOnCall[len(fake.commitArgsForCall)]
	fake.commitArgsForCall = append(fake.commitArgsForCall, struct {
	}{})
	stub := fake.CommitStub
	fakeReturns := fake.commitReturns
	fake.recordInvocation("Commit", []interface{}{})
	fake.commitMutex.Unlock()
	if stub != nil {
		return stub()
	}
	if specificReturn {
		return ret.result1
	}
	return fakeReturns.result1
}

func (fake *FakeTx) CommitCallCount() int {
	fake.commitMutex.RLock()
	defer fake.commitMutex.RUnlock()
	return len(fake.commitArgsForCall)
}

func (fake *FakeTx) CommitCalls(stub func() error) {
	fake.commitMutex.Lock()
	defer fake.commitMutex.Unlock()
	fake.CommitStub = stub
}

func (fake *FakeTx) CommitReturns(result1 error) {
	fake.commitMutex.Lock()
	defer fake.commitMutex.Unlock()
	fake.CommitStub = nil
	fake.commitReturns = struct {
		result1 error
	}{result1}
}

func (fake *FakeTx) CommitReturnsOnCall(i int, result1 error) {
	fake.commitMutex.Lock()
	defer fake.commitMutex.Unlock()
	fake.CommitStub = nil
	if fake.commitReturnsOnCall == nil {
		fake.commitReturnsOnCall = make(map[int]struct {
			result1 error
		})
	}
	fake.commitReturnsOnCall[i] = struct {
		result1 error
	}{result1}
}

func (fake *FakeTx) EncryptionStrategy() encryption.Strategy {
	fake.encryptionStrategyMutex.Lock()
	ret, specificReturn := fake.encryptionStrategyReturnsOnCall[len(fake.encryptionStrategyArgsForCall)]
	fake.encryptionStrategyArgsForCall = append(fake.encryptionStrategyArgsForCall, struct {
	}{})
	stub := fake.EncryptionStrategyStub
	fakeReturns := fake.encryptionStrategyReturns
	fake.recordInvocation("EncryptionStrategy", []interface{}{})
	fake.encryptionStrategyMutex.Unlock()
	if stub != nil {
		return stub()
	}
	if specificReturn {
		return ret.result1
	}
	return fakeReturns.result1
}

func (fake *FakeTx) EncryptionStrategyCallCount() int {
	fake.encryptionStrategyMutex.RLock()
	defer fake.encryptionStrategyMutex.RUnlock()
	return len(fake.encryptionStrategyArgsForCall)
}

func (fake *FakeTx) EncryptionStrategyCalls(stub func() encryption.Strategy) {
	fake.encryptionStrategyMutex.Lock()
	defer fake.encryptionStrategyMutex.Unlock()
	fake.EncryptionStrategyStub = stub
}

func (fake *FakeTx) EncryptionStrategyReturns(result1 encryption.Strategy) {
	fake.encryptionStrategyMutex.Lock()
	defer fake.encryptionStrategyMutex.Unlock()
	fake.EncryptionStrategyStub = nil
	fake.encryptionStrategyReturns = struct {
		result1 encryption.Strategy
	}{result1}
}

func (fake *FakeTx) EncryptionStrategyReturnsOnCall(i int, result1 encryption.Strategy) {
	fake.encryptionStrategyMutex.Lock()
	defer fake.encryptionStrategyMutex.Unlock()
	fake.EncryptionStrategyStub = nil
	if fake.encryptionStrategyReturnsOnCall == nil {
		fake.encryptionStrategyReturnsOnCall = make(map[int]struct {
			result1 encryption.Strategy
		})
	}
	fake.encryptionStrategyReturnsOnCall[i] = struct {
		result1 encryption.Strategy
	}{result1}
}

func (fake *FakeTx) Exec(arg1 string, arg2 ...interface{}) (sql.Result, error) {
	fake.execMutex.Lock()
	ret, specificReturn := fake.execReturnsOnCall[len(fake.execArgsForCall)]
	fake.execArgsForCall = append(fake.execArgsForCall, struct {
		arg1 string
		arg2 []interface{}
	}{arg1, arg2})
	stub := fake.ExecStub
	fakeReturns := fake.execReturns
	fake.recordInvocation("Exec", []interface{}{arg1, arg2})
	fake.execMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2...)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeTx) ExecCallCount() int {
	fake.execMutex.RLock()
	defer fake.execMutex.RUnlock()
	return len(fake.execArgsForCall)
}

func (fake *FakeTx) ExecCalls(stub func(string, ...interface{}) (sql.Result, error)) {
	fake.execMutex.Lock()
	defer fake.execMutex.Unlock()
	fake.ExecStub = stub
}

func (fake *FakeTx) ExecArgsForCall(i int) (string, []interface{}) {
	fake.execMutex.RLock()
	defer fake.execMutex.RUnlock()
	argsForCall := fake.execArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2
}

func (fake *FakeTx) ExecReturns(result1 sql.Result, result2 error) {
	fake.execMutex.Lock()
	defer fake.execMutex.Unlock()
	fake.ExecStub = nil
	fake.execReturns = struct {
		result1 sql.Result
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) ExecReturnsOnCall(i int, result1 sql.Result, result2 error) {
	fake.execMutex.Lock()
	defer fake.execMutex.Unlock()
	fake.ExecStub = nil
	if fake.execReturnsOnCall == nil {
		fake.execReturnsOnCall = make(map[int]struct {
			result1 sql.Result
			result2 error
		})
	}
	fake.execReturnsOnCall[i] = struct {
		result1 sql.Result
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) ExecContext(arg1 context.Context, arg2 string, arg3 ...interface{}) (sql.Result, error) {
	fake.execContextMutex.Lock()
	ret, specificReturn := fake.execContextReturnsOnCall[len(fake.execContextArgsForCall)]
	fake.execContextArgsForCall = append(fake.execContextArgsForCall, struct {
		arg1 context.Context
		arg2 string
		arg3 []interface{}
	}{arg1, arg2, arg3})
	stub := fake.ExecContextStub
	fakeReturns := fake.execContextReturns
	fake.recordInvocation("ExecContext", []interface{}{arg1, arg2, arg3})
	fake.execContextMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2, arg3...)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeTx) ExecContextCallCount() int {
	fake.execContextMutex.RLock()
	defer fake.execContextMutex.RUnlock()
	return len(fake.execContextArgsForCall)
}

func (fake *FakeTx) ExecContextCalls(stub func(context.Context, string, ...interface{}) (sql.Result, error)) {
	fake.execContextMutex.Lock()
	defer fake.execContextMutex.Unlock()
	fake.ExecContextStub = stub
}

func (fake *FakeTx) ExecContextArgsForCall(i int) (context.Context, string, []interface{}) {
	fake.execContextMutex.RLock()
	defer fake.execContextMutex.RUnlock()
	argsForCall := fake.execContextArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3
}

func (fake *FakeTx) ExecContextReturns(result1 sql.Result, result2 error) {
	fake.execContextMutex.Lock()
	defer fake.execContextMutex.Unlock()
	fake.ExecContextStub = nil
	fake.execContextReturns = struct {
		result1 sql.Result
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) ExecContextReturnsOnCall(i int, result1 sql.Result, result2 error) {
	fake.execContextMutex.Lock()
	defer fake.execContextMutex.Unlock()
	fake.ExecContextStub = nil
	if fake.execContextReturnsOnCall == nil {
		fake.execContextReturnsOnCall = make(map[int]struct {
			result1 sql.Result
			result2 error
		})
	}
	fake.execContextReturnsOnCall[i] = struct {
		result1 sql.Result
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) Prepare(arg1 string) (*sql.Stmt, error) {
	fake.prepareMutex.Lock()
	ret, specificReturn := fake.prepareReturnsOnCall[len(fake.prepareArgsForCall)]
	fake.prepareArgsForCall = append(fake.prepareArgsForCall, struct {
		arg1 string
	}{arg1})
	stub := fake.PrepareStub
	fakeReturns := fake.prepareReturns
	fake.recordInvocation("Prepare", []interface{}{arg1})
	fake.prepareMutex.Unlock()
	if stub != nil {
		return stub(arg1)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeTx) PrepareCallCount() int {
	fake.prepareMutex.RLock()
	defer fake.prepareMutex.RUnlock()
	return len(fake.prepareArgsForCall)
}

func (fake *FakeTx) PrepareCalls(stub func(string) (*sql.Stmt, error)) {
	fake.prepareMutex.Lock()
	defer fake.prepareMutex.Unlock()
	fake.PrepareStub = stub
}

func (fake *FakeTx) PrepareArgsForCall(i int) string {
	fake.prepareMutex.RLock()
	defer fake.prepareMutex.RUnlock()
	argsForCall := fake.prepareArgsForCall[i]
	return argsForCall.arg1
}

func (fake *FakeTx) PrepareReturns(result1 *sql.Stmt, result2 error) {
	fake.prepareMutex.Lock()
	defer fake.prepareMutex.Unlock()
	fake.PrepareStub = nil
	fake.prepareReturns = struct {
		result1 *sql.Stmt
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) PrepareReturnsOnCall(i int, result1 *sql.Stmt, result2 error) {
	fake.prepareMutex.Lock()
	defer fake.prepareMutex.Unlock()
	fake.PrepareStub = nil
	if fake.prepareReturnsOnCall == nil {
		fake.prepareReturnsOnCall = make(map[int]struct {
			result1 *sql.Stmt
			result2 error
		})
	}
	fake.prepareReturnsOnCall[i] = struct {
		result1 *sql.Stmt
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) PrepareContext(arg1 context.Context, arg2 string) (*sql.Stmt, error) {
	fake.prepareContextMutex.Lock()
	ret, specificReturn := fake.prepareContextReturnsOnCall[len(fake.prepareContextArgsForCall)]
	fake.prepareContextArgsForCall = append(fake.prepareContextArgsForCall, struct {
		arg1 context.Context
		arg2 string
	}{arg1, arg2})
	stub := fake.PrepareContextStub
	fakeReturns := fake.prepareContextReturns
	fake.recordInvocation("PrepareContext", []interface{}{arg1, arg2})
	fake.prepareContextMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeTx) PrepareContextCallCount() int {
	fake.prepareContextMutex.RLock()
	defer fake.prepareContextMutex.RUnlock()
	return len(fake.prepareContextArgsForCall)
}

func (fake *FakeTx) PrepareContextCalls(stub func(context.Context, string) (*sql.Stmt, error)) {
	fake.prepareContextMutex.Lock()
	defer fake.prepareContextMutex.Unlock()
	fake.PrepareContextStub = stub
}

func (fake *FakeTx) PrepareContextArgsForCall(i int) (context.Context, string) {
	fake.prepareContextMutex.RLock()
	defer fake.prepareContextMutex.RUnlock()
	argsForCall := fake.prepareContextArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2
}

func (fake *FakeTx) PrepareContextReturns(result1 *sql.Stmt, result2 error) {
	fake.prepareContextMutex.Lock()
	defer fake.prepareContextMutex.Unlock()
	fake.PrepareContextStub = nil
	fake.prepareContextReturns = struct {
		result1 *sql.Stmt
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) PrepareContextReturnsOnCall(i int, result1 *sql.Stmt, result2 error) {
	fake.prepareContextMutex.Lock()
	defer fake.prepareContextMutex.Unlock()
	fake.PrepareContextStub = nil
	if fake.prepareContextReturnsOnCall == nil {
		fake.prepareContextReturnsOnCall = make(map[int]struct {
			result1 *sql.Stmt
			result2 error
		})
	}
	fake.prepareContextReturnsOnCall[i] = struct {
		result1 *sql.Stmt
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) Query(arg1 string, arg2 ...interface{}) (*sql.Rows, error) {
	fake.queryMutex.Lock()
	ret, specificReturn := fake.queryReturnsOnCall[len(fake.queryArgsForCall)]
	fake.queryArgsForCall = append(fake.queryArgsForCall, struct {
		arg1 string
		arg2 []interface{}
	}{arg1, arg2})
	stub := fake.QueryStub
	fakeReturns := fake.queryReturns
	fake.recordInvocation("Query", []interface{}{arg1, arg2})
	fake.queryMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2...)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeTx) QueryCallCount() int {
	fake.queryMutex.RLock()
	defer fake.queryMutex.RUnlock()
	return len(fake.queryArgsForCall)
}

func (fake *FakeTx) QueryCalls(stub func(string, ...interface{}) (*sql.Rows, error)) {
	fake.queryMutex.Lock()
	defer fake.queryMutex.Unlock()
	fake.QueryStub = stub
}

func (fake *FakeTx) QueryArgsForCall(i int) (string, []interface{}) {
	fake.queryMutex.RLock()
	defer fake.queryMutex.RUnlock()
	argsForCall := fake.queryArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2
}

func (fake *FakeTx) QueryReturns(result1 *sql.Rows, result2 error) {
	fake.queryMutex.Lock()
	defer fake.queryMutex.Unlock()
	fake.QueryStub = nil
	fake.queryReturns = struct {
		result1 *sql.Rows
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) QueryReturnsOnCall(i int, result1 *sql.Rows, result2 error) {
	fake.queryMutex.Lock()
	defer fake.queryMutex.Unlock()
	fake.QueryStub = nil
	if fake.queryReturnsOnCall == nil {
		fake.queryReturnsOnCall = make(map[int]struct {
			result1 *sql.Rows
			result2 error
		})
	}
	fake.queryReturnsOnCall[i] = struct {
		result1 *sql.Rows
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) QueryContext(arg1 context.Context, arg2 string, arg3 ...interface{}) (*sql.Rows, error) {
	fake.queryContextMutex.Lock()
	ret, specificReturn := fake.queryContextReturnsOnCall[len(fake.queryContextArgsForCall)]
	fake.queryContextArgsForCall = append(fake.queryContextArgsForCall, struct {
		arg1 context.Context
		arg2 string
		arg3 []interface{}
	}{arg1, arg2, arg3})
	stub := fake.QueryContextStub
	fakeReturns := fake.queryContextReturns
	fake.recordInvocation("QueryContext", []interface{}{arg1, arg2, arg3})
	fake.queryContextMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2, arg3...)
	}
	if specificReturn {
		return ret.result1, ret.result2
	}
	return fakeReturns.result1, fakeReturns.result2
}

func (fake *FakeTx) QueryContextCallCount() int {
	fake.queryContextMutex.RLock()
	defer fake.queryContextMutex.RUnlock()
	return len(fake.queryContextArgsForCall)
}

func (fake *FakeTx) QueryContextCalls(stub func(context.Context, string, ...interface{}) (*sql.Rows, error)) {
	fake.queryContextMutex.Lock()
	defer fake.queryContextMutex.Unlock()
	fake.QueryContextStub = stub
}

func (fake *FakeTx) QueryContextArgsForCall(i int) (context.Context, string, []interface{}) {
	fake.queryContextMutex.RLock()
	defer fake.queryContextMutex.RUnlock()
	argsForCall := fake.queryContextArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3
}

func (fake *FakeTx) QueryContextReturns(result1 *sql.Rows, result2 error) {
	fake.queryContextMutex.Lock()
	defer fake.queryContextMutex.Unlock()
	fake.QueryContextStub = nil
	fake.queryContextReturns = struct {
		result1 *sql.Rows
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) QueryContextReturnsOnCall(i int, result1 *sql.Rows, result2 error) {
	fake.queryContextMutex.Lock()
	defer fake.queryContextMutex.Unlock()
	fake.QueryContextStub = nil
	if fake.queryContextReturnsOnCall == nil {
		fake.queryContextReturnsOnCall = make(map[int]struct {
			result1 *sql.Rows
			result2 error
		})
	}
	fake.queryContextReturnsOnCall[i] = struct {
		result1 *sql.Rows
		result2 error
	}{result1, result2}
}

func (fake *FakeTx) QueryRow(arg1 string, arg2 ...interface{}) squirrel.RowScanner {
	fake.queryRowMutex.Lock()
	ret, specificReturn := fake.queryRowReturnsOnCall[len(fake.queryRowArgsForCall)]
	fake.queryRowArgsForCall = append(fake.queryRowArgsForCall, struct {
		arg1 string
		arg2 []interface{}
	}{arg1, arg2})
	stub := fake.QueryRowStub
	fakeReturns := fake.queryRowReturns
	fake.recordInvocation("QueryRow", []interface{}{arg1, arg2})
	fake.queryRowMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2...)
	}
	if specificReturn {
		return ret.result1
	}
	return fakeReturns.result1
}

func (fake *FakeTx) QueryRowCallCount() int {
	fake.queryRowMutex.RLock()
	defer fake.queryRowMutex.RUnlock()
	return len(fake.queryRowArgsForCall)
}

func (fake *FakeTx) QueryRowCalls(stub func(string, ...interface{}) squirrel.RowScanner) {
	fake.queryRowMutex.Lock()
	defer fake.queryRowMutex.Unlock()
	fake.QueryRowStub = stub
}

func (fake *FakeTx) QueryRowArgsForCall(i int) (string, []interface{}) {
	fake.queryRowMutex.RLock()
	defer fake.queryRowMutex.RUnlock()
	argsForCall := fake.queryRowArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2
}

func (fake *FakeTx) QueryRowReturns(result1 squirrel.RowScanner) {
	fake.queryRowMutex.Lock()
	defer fake.queryRowMutex.Unlock()
	fake.QueryRowStub = nil
	fake.queryRowReturns = struct {
		result1 squirrel.RowScanner
	}{result1}
}

func (fake *FakeTx) QueryRowReturnsOnCall(i int, result1 squirrel.RowScanner) {
	fake.queryRowMutex.Lock()
	defer fake.queryRowMutex.Unlock()
	fake.QueryRowStub = nil
	if fake.queryRowReturnsOnCall == nil {
		fake.queryRowReturnsOnCall = make(map[int]struct {
			result1 squirrel.RowScanner
		})
	}
	fake.queryRowReturnsOnCall[i] = struct {
		result1 squirrel.RowScanner
	}{result1}
}

func (fake *FakeTx) QueryRowContext(arg1 context.Context, arg2 string, arg3 ...interface{}) squirrel.RowScanner {
	fake.queryRowContextMutex.Lock()
	ret, specificReturn := fake.queryRowContextReturnsOnCall[len(fake.queryRowContextArgsForCall)]
	fake.queryRowContextArgsForCall = append(fake.queryRowContextArgsForCall, struct {
		arg1 context.Context
		arg2 string
		arg3 []interface{}
	}{arg1, arg2, arg3})
	stub := fake.QueryRowContextStub
	fakeReturns := fake.queryRowContextReturns
	fake.recordInvocation("QueryRowContext", []interface{}{arg1, arg2, arg3})
	fake.queryRowContextMutex.Unlock()
	if stub != nil {
		return stub(arg1, arg2, arg3...)
	}
	if specificReturn {
		return ret.result1
	}
	return fakeReturns.result1
}

func (fake *FakeTx) QueryRowContextCallCount() int {
	fake.queryRowContextMutex.RLock()
	defer fake.queryRowContextMutex.RUnlock()
	return len(fake.queryRowContextArgsForCall)
}

func (fake *FakeTx) QueryRowContextCalls(stub func(context.Context, string, ...interface{}) squirrel.RowScanner) {
	fake.queryRowContextMutex.Lock()
	defer fake.queryRowContextMutex.Unlock()
	fake.QueryRowContextStub = stub
}

func (fake *FakeTx) QueryRowContextArgsForCall(i int) (context.Context, string, []interface{}) {
	fake.queryRowContextMutex.RLock()
	defer fake.queryRowContextMutex.RUnlock()
	argsForCall := fake.queryRowContextArgsForCall[i]
	return argsForCall.arg1, argsForCall.arg2, argsForCall.arg3
}

func (fake *FakeTx) QueryRowContextReturns(result1 squirrel.RowScanner) {
	fake.queryRowContextMutex.Lock()
	defer fake.queryRowContextMutex.Unlock()
	fake.QueryRowContextStub = nil
	fake.queryRowContextReturns = struct {
		result1 squirrel.RowScanner
	}{result1}
}

func (fake *FakeTx) QueryRowContextReturnsOnCall(i int, result1 squirrel.RowScanner) {
	fake.queryRowContextMutex.Lock()
	defer fake.queryRowContextMutex.Unlock()
	fake.QueryRowContextStub = nil
	if fake.queryRowContextReturnsOnCall == nil {
		fake.queryRowContextReturnsOnCall = make(map[int]struct {
			result1 squirrel.RowScanner
		})
	}
	fake.queryRowContextReturnsOnCall[i] = struct {
		result1 squirrel.RowScanner
	}{result1}
}

func (fake *FakeTx) Rollback() error {
	fake.rollbackMutex.Lock()
	ret, specificReturn := fake.rollbackReturnsOnCall[len(fake.rollbackArgsForCall)]
	fake.rollbackArgsForCall = append(fake.rollbackArgsForCall, struct {
	}{})
	stub := fake.RollbackStub
	fakeReturns := fake.rollbackReturns
	fake.recordInvocation("Rollback", []interface{}{})
	fake.rollbackMutex.Unlock()
	if stub != nil {
		return stub()
	}
	if specificReturn {
		return ret.result1
	}
	return fakeReturns.result1
}

func (fake *FakeTx) RollbackCallCount() int {
	fake.rollbackMutex.RLock()
	defer fake.rollbackMutex.RUnlock()
	return len(fake.rollbackArgsForCall)
}

func (fake *FakeTx) RollbackCalls(stub func() error) {
	fake.rollbackMutex.Lock()
	defer fake.rollbackMutex.Unlock()
	fake.RollbackStub = stub
}

func (fake *FakeTx) RollbackReturns(result1 error) {
	fake.rollbackMutex.Lock()
	defer fake.rollbackMutex.Unlock()
	fake.RollbackStub = nil
	fake.rollbackReturns = struct {
		result1 error
	}{result1}
}

func (fake *FakeTx) RollbackReturnsOnCall(i int, result1 error) {
	fake.rollbackMutex.Lock()
	defer fake.rollbackMutex.Unlock()
	fake.RollbackStub = nil
	if fake.rollbackReturnsOnCall == nil {
		fake.rollbackReturnsOnCall = make(map[int]struct {
			result1 error
		})
	}
	fake.rollbackReturnsOnCall[i] = struct {
		result1 error
	}{result1}
}

func (fake *FakeTx) Stmt(arg1 *sql.Stmt) *sql.Stmt {
	fake.stmtMutex.Lock()
	ret, specificReturn := fake.stmtReturnsOnCall[len(fake.stmtArgsForCall)]
	fake.stmtArgsForCall = append(fake.stmtArgsForCall, struct {
		arg1 *sql.Stmt
	}{arg1})
	stub := fake.StmtStub
	fakeReturns := fake.stmtReturns
	fake.recordInvocation("Stmt", []interface{}{arg1})
	fake.stmtMutex.Unlock()
	if stub != nil {
		return stub(arg1)
	}
	if specificReturn {
		return ret.result1
	}
	return fakeReturns.result1
}

func (fake *FakeTx) StmtCallCount() int {
	fake.stmtMutex.RLock()
	defer fake.stmtMutex.RUnlock()
	return len(fake.stmtArgsForCall)
}

func (fake *FakeTx) StmtCalls(stub func(*sql.Stmt) *sql.Stmt) {
	fake.stmtMutex.Lock()
	defer fake.stmtMutex.Unlock()
	fake.StmtStub = stub
}

func (fake *FakeTx) StmtArgsForCall(i int) *sql.Stmt {
	fake.stmtMutex.RLock()
	defer fake.stmtMutex.RUnlock()
	argsForCall := fake.stmtArgsForCall[i]
	return argsForCall.arg1
}

func (fake *FakeTx) StmtReturns(result1 *sql.Stmt) {
	fake.stmtMutex.Lock()
	defer fake.stmtMutex.Unlock()
	fake.StmtStub = nil
	fake.stmtReturns = struct {
		result1 *sql.Stmt
	}{result1}
}

func (fake *FakeTx) StmtReturnsOnCall(i int, result1 *sql.Stmt) {
	fake.stmtMutex.Lock()
	defer fake.stmtMutex.Unlock()
	fake.StmtStub = nil
	if fake.stmtReturnsOnCall == nil {
		fake.stmtReturnsOnCall = make(map[int]struct {
			result1 *sql.Stmt
		})
	}
	fake.stmtReturnsOnCall[i] = struct {
		result1 *sql.Stmt
	}{result1}
}

func (fake *FakeTx) Invocations() map[string][][]interface{} {
	fake.invocationsMutex.RLock()
	defer fake.invocationsMutex.RUnlock()
	fake.commitMutex.RLock()
	defer fake.commitMutex.RUnlock()
	fake.encryptionStrategyMutex.RLock()
	defer fake.encryptionStrategyMutex.RUnlock()
	fake.execMutex.RLock()
	defer fake.execMutex.RUnlock()
	fake.execContextMutex.RLock()
	defer fake.execContextMutex.RUnlock()
	fake.prepareMutex.RLock()
	defer fake.prepareMutex.RUnlock()
	fake.prepareContextMutex.RLock()
	defer fake.prepareContextMutex.RUnlock()
	fake.queryMutex.RLock()
	defer fake.queryMutex.RUnlock()
	fake.queryContextMutex.RLock()
	defer fake.queryContextMutex.RUnlock()
	fake.queryRowMutex.RLock()
	defer fake.queryRowMutex.RUnlock()
	fake.queryRowContextMutex.RLock()
	defer fake.queryRowContextMutex.RUnlock()
	fake.rollbackMutex.RLock()
	defer fake.rollbackMutex.RUnlock()
	fake.stmtMutex.RLock()
	defer fake.stmtMutex.RUnlock()
	copiedInvocations := map[string][][]interface{}{}
	for key, value := range fake.invocations {
		copiedInvocations[key] = value
	}
	return copiedInvocations
}

func (fake *FakeTx) recordInvocation(key string, args []interface{}) {
	fake.invocationsMutex.Lock()
	defer fake.invocationsMutex.Unlock()
	if fake.invocations == nil {
		fake.invocations = map[string][][]interface{}{}
	}
	if fake.invocations[key] == nil {
		fake.invocations[key] = [][]interface{}{}
	}
	fake.invocations[key] = append(fake.invocations[key], args)
}

var _ db.Tx = new(FakeTx)
